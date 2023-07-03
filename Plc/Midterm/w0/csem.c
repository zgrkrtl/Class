

#include <stdio.h>

int main (void)
{

    int *a, *b;
    int num1, num2;

    num1 = 10;
    num2 = 30;

    a = &num1;
    b = &num2;

    printf("%p\n", &a);
    printf("%p\n", &b);

    printf("%p\n", a);
    printf("%p\n", b);

    printf("%p\n", &num1);
    printf("%p\n", &num2);   

    printf("%d\n", num1);
    printf("%d\n", num2);

    printf("-- %d\n", *a);
    printf("-- %d\n", *b);

    printf("%d\n", &a == &b);
    printf("%d\n", a == b);
    printf("%d\n", *a == *b);


    return 0;

}
