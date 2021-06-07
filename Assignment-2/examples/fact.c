
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

typedef struct Closure {
    void* (*func)(void*, void**);
    void** env;
} Clo;

Clo* lam(void* (*func)(void*, void**), void** env) {
    Clo* clo = (Clo*)malloc(sizeof(Clo));
    clo->func = func;
    clo->env = env;
    return clo;
}

void** ext(void* arg, void** env, int len) {
    void** new_env = (void**)malloc(sizeof(void*)*len+1);
    new_env[0] = arg;
    memcpy(new_env+1, env, len);
    return new_env;
}

void* app(void* c, void* arg) {
    Clo* clo = (Clo *)c;
    return (*(clo->func))(arg, clo->env);
}

void* lambda664(void* v, void** env) {
    return ((long) v <= 0 ? 1 : ((long) v * (long) app(app(env[0 /* self */], env[0 /* self */]), ((long) v + (long) -1))));
}
 
void* lambda663(void* self, void** env) {
    return lam(&lambda664, ext(self, env, 0));
}
 
void* lambda662(void* fact, void** env) {
    return app(app(fact, fact), 10);
}

int main() {
    return printf("%d\n", (long) app(lam(&lambda662, NULL), lam(&lambda663, NULL)));
}
