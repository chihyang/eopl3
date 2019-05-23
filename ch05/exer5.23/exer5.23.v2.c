#include <assert.h>
#include <stdio.h>

int x = 13;
int proc() {
    goto odd;
even:
    if (x == 0) {
        return 1;
    } else {
        x = x - 1;
        goto odd;
    }
odd:
    if (x == 0) {
        return 0;
    } else {
        x = x - 1;
    }
}

int main()
{
    int ret = proc();
    assert(x == 12);
    printf("final register value: %d, final answer: %d\n", x, ret);
    return 0;
}
