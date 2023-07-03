#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"
#include "pthread.h"
#include "semaphore.h"
#include "time.h"
#include "stdbool.h"

#define NUMOP 5
#define TLIMIT 5
#define LEAVET 20000000000
#define MAXWT 1000000

struct board
{
    sem_t cs[NUMOP];
    long WT[NUMOP];
    long TWT[NUMOP];
    bool isLeft[NUMOP];
};

typedef struct board BOARD;
BOARD b;

long getSystemTime(void);
void initBoard(void);
void philosophers(int *);
bool canRun(int);
void updateOthersWT(int, long);

int main(int argc, char const *argv[])
{
    pthread_t t[NUMOP];
    int id[NUMOP];

    initBoard();

    for (size_t i = 0; i < NUMOP; i++)
    {
        id[i] = i;
        pthread_create(&t[i], NULL, (void *) philosophers, &id[i]);
    }

    for (size_t i = 0; i < NUMOP; i++)
        pthread_join(t[i], NULL);

    printf("All philosophers have left...\n");  
    

    return 0;
}

long getSystemTime(void)
{
    struct timespec ts;

    if(clock_gettime(1, &ts) == 0) 
        return (long) ts.tv_sec * 1000000000 + ts.tv_nsec;

    return 0;
}

void initBoard(void)
{
    for (size_t i = 0; i < NUMOP; i++)
    {
        sem_init(&b.cs[i], 1, 1);
        b.WT[i] = 0;
        b.TWT[i] = 0;
        b.isLeft[i] = false;
    }
}

void philosophers(int *identity)
{
    int id = *identity;

    while (true)
    {
        srand(time(NULL) * id);
        long start = getSystemTime();
        int thinkTime = (rand() % TLIMIT) + 1;
        printf("philosopher %d is thinking for %d seconds\n", id, thinkTime);
        sleep(thinkTime);

        long now = getSystemTime();
        printf("philosopher %d is hungry\n", id);
        b.WT[id] = b.WT[id] + (now - start);
        b.TWT[id] = b.TWT[id] + b.WT[id];

        long check = getSystemTime();

        if(canRun(id))
        {
            sem_wait(&b.cs[id]);
            sem_wait(&b.cs[(id+1)%NUMOP]);
            int eatingTime = (rand() % TLIMIT) + 1;
            printf("philosopher %d is ***EATING*** for %d seconds\n", id, eatingTime);
            sleep(eatingTime);
            b.WT[id] = 0;
            updateOthersWT(id, check);
            
            if(b.TWT[id] >= LEAVET)
            {
                printf("philosopher %d is LEAVING the table\n", id);
                b.isLeft[id] = true;
                sem_post(&b.cs[(id+1)%NUMOP]);
                sem_post(&b.cs[id]);
                pthread_exit(NULL);
            }
            else
            {
                sem_post(&b.cs[(id+1)%NUMOP]);
                sem_post(&b.cs[id]);
            }
        }

    }   
}

void updateOthersWT(int id, long check)
{
    long now = getSystemTime();
    for (size_t i = 0; i < NUMOP && i != id; i++)
    {
        b.WT[i] = b.WT[i] + (now - check);
        b.TWT[i] = b.TWT[i] + b.WT[i];
    }
}

bool canRun(int id)
{
    long current = b.WT[id];

    long left = b.WT[(id+NUMOP-1)%NUMOP];

    long right = b.WT[(id+1)%NUMOP];

    if(left > current && left > MAXWT && !b.isLeft[(id+NUMOP-1)%NUMOP])
    {
        printf("philosopher %d is giving way to the philosopher %d to avoid starvation\n", id, (id+NUMOP-1)%NUMOP);
        return false;
    }
    if(right > current && right > MAXWT && !b.isLeft[(id+1)%NUMOP])
    {
        printf("philosopher %d is giving way to the philosopher %d to avoid starvation\n", id, (id+1)%NUMOP);
        return false;
    }
    return true;
}








