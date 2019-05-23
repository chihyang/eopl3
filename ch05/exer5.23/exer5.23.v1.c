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
    }
odd:
    if (x == 0) {
        return 0;
    } else {
        x = x - 1;
        goto even;
    }
}

int main()
{
    int ret = proc();
    assert(x == 0);
    printf("final register value: %d, final answer: %d\n", x, ret);
    return 0;
}
