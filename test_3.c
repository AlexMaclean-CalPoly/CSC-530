#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

typedef struct Closure {
    void* (*func)(void**);
    void** env;
    int len;
} Clo;

Clo* create_closure(void* (*func)(void**), void** env, int len) {
    Clo* clo = (Clo*)malloc(sizeof(Clo));
    clo->func = func;
    clo->env = (void**)malloc(sizeof(void*)*(len+1));
    memcpy(clo->env+1, env, len);
    clo->len = len;
    return clo;
}

void* run_closure(void* c, void* arg) {
    Clo* clo = (Clo *)c;
    clo->env[0] = arg;
    return (*(clo->func))(clo->env);
}
void* lambda6605(void** env) {
    return ((int) env[0 /* v */] <= 0 ? 1 : ((int) env[0 /* v */] * (int) run_closure(run_closure(env[1 /* self */], env[1 /* self */]), ((int) env[0 /* v */] + (int) -1))));
}

void* lambda6603(void** env) {
    return run_closure(run_closure(env[0 /* fact */], env[0 /* fact */]), 10);
}

void* lambda6604(void** env) {
    return create_closure(&lambda6605, env, 1);
}

int main() {
    void* env = 0;
    return printf("%d\n", (int) run_closure(create_closure(&lambda6603, env, 0), create_closure(&lambda6604, env, 0)));
}

