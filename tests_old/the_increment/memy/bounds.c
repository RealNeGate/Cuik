#include <stdlib.h>
#include <stdint.h>

typedef struct {
    char* ptr;

    // this is the space we're allowed to dereference
    uint32_t limit;
    // you can access one element past a memory object
    uint32_t ref_limit;
} __Cuik_MemoryObj;

enum {
    __CUIK_MAX_STACK = 0x10000,
    __CUIK_MAX_HEAP = 0x10000,
};

size_t __cuik_stack_obj_ptr = 1;
__Cuik_MemoryObj __cuik_objects[__CUIK_MAX_STACK + __CUIK_MAX_HEAP];

[[nomemy]] size_t __cuik_get_stack(void) {
    return __cuik_stack_obj_ptr;
}

[[nomemy]] void __cuik_restore_stack(size_t a) {
    __cuik_stack_obj_ptr = a;
}

[[nomemy]] __Cuik_MemoryObj* __cuik_push_stack_obj(__Cuik_MemoryObj* base, size_t limit, size_t ref_limit) {
    __Cuik_MemoryObj* obj = &__cuik_objects[__cuik_stack_obj_ptr++];
    obj->base = base;
    obj->limit = limit;
    obj->ref_limit = ref_limit;
    return obj;
}

[[nomemy]] void* __cuik_resolve_ptr(uint64_t ref) {
    __Cuik_MemoryObj* obj = &__cuik_objects[ref & 0xFFFFFFFF00000000];
    if (obj->ptr == NULL) return NULL;

    return &obj->base[ref & 0xFFFFFFFF];
}

int main(void) {
    // memory object 'a' with 12 bytes of space
    int a[3] = { 0 };

    // legal
    int* b = &a[3];

    // legal
    int c = a[2];

    // illegal
    int d = a[3];

    return d;
}
