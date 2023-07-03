#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"
#include "pthread.h"
#include "stdbool.h"
#include "syscall.h"

#define NUMOFP 5

struct board
{
    bool flag;
    int x;
};

typedef struct board BOARD;

void initBoard(BOARD *);
void process1(BOARD *);
void process2(BOARD *);


int main(int argc, char const *argv[])
{
    BOARD *b = malloc(sizeof(BOARD));

    initBoard(b);
    pthread_t t1, t2;

    pthread_create(&t1, NULL, (void *) process1, b);
    pthread_create(&t2, NULL, (void *) process2, b);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    return 0;
}

void initBoard(BOARD *b)
{
     b->flag = false;
     b->x = 0;
}

void process1(BOARD *b)
{
    while (!b->flag)
        SYS_membarrier;
    printf("x = %d\n", b->x);
}

void process2(BOARD *b)
{

    b->x = 100;
    SYS_membarrier;
    b->flag = true;
}