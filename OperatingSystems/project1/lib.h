#pragma once

#include <stddef.h>

void *foo_malloc(size_t size);
void *foo_calloc(size_t count, size_t size);
void *foo_realloc(void *ptr, size_t size);
void foo_free(void *ptr);
