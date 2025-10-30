#pragma once

#include <assert.h>

#ifdef _WIN32
    #ifdef _DEBUG
    #define malloc(s) _malloc_dbg(s, _NORMAL_BLOCK, __FILE__, __LINE__)
    #define free(p) _free_dbg(p, _NORMAL_BLOCK)
    #endif
#elif __linux__
    #include <stdint.h>
    #include <cstring>

#endif

#ifdef _DEBUG
    extern int totalNbyte;
#endif


// const size_t POOL_BUCKET_SIZE_DEFAULT = 65536; // 64 KiB
const size_t POOL_BUCKET_SIZE_DEFAULT = 128*1024;

struct Pool;

inline void pool_init(Pool *pool);
inline void *pool_alloc(Pool *pool, size_t size);
inline void ensure_memory_exists(Pool *pool, size_t size);
inline void resize_blocks(Pool *pool, size_t block_size);
inline void cycle_new_block(Pool *pool);
inline void pool_reset(Pool *pool);
inline void pool_release(Pool *pool);

template<typename T>
struct Array
{
    T *data;
    long count;
    long capacity;

    Pool *pool;

    Array() : data(nullptr), count(0), capacity(0), pool(nullptr) {}
    Array(Pool *p) : data(nullptr), count(0), capacity(0), pool(p) {}

    void push_back(T value);
    T pop();
    void release();

    void pop_back();

    // inline helpers
    bool empty() const { return count == 0; }
    long size() const { return count; }
};

template<typename T>
inline void Array<T>::push_back(T value)
{

#ifdef _DEBUG
   printf("\n<<<<<<<< PUSH_BACK >>>>>>>>>>\n\n");
#endif

    assert(pool && "Array's pool pointer is null!");

    if (count >= capacity) {
        long new_cap = capacity ? capacity * 2 : 4;

        T *new_data = (T *)pool_alloc(pool, sizeof(T) * new_cap);
        assert(new_data && "Memory allocation failed for Array");

        if (data) {
            memcpy(new_data, data, sizeof(T) * count);
        }
        data = new_data;
        capacity = new_cap;
    }
   else {

    #ifdef _DEBUG
       printf("\n<<<<<<<< PUSH_BACK ___FAILEDD___ >>>>>>>>>>\n\n");
    #endif

   }
    data[count++] = value;
}

template<typename T>
inline T Array<T>::pop()
{
    if (count == 0) {
        printf("*********** ATTEMPT TO POP AN EMPTY ARRAY!\n");
        return T();
    }

    return data[count-1];
}

template<typename T>
inline void Array<T>::release()
{
    data = nullptr;
    count = 0;
    capacity = 0;
}

template<typename T>
inline void Array<T>::pop_back() {
    assert(count > 0 && "Attempt to pop_back from empty Array!");
    count--;

    // Explicit destructor call only if non-trivial type
    data[count].~T();
}


// we need to keep track of used, unused, obsolete blocks and need to use malloc seperately where we push starting positions of these blocks

// update this to make it simpler!!!!!!!!!!!!!!
struct BlockList
{
    void **data = nullptr;
    long count = 0;
    long capacity = 0;
};


inline void blocklist_push(BlockList *list, void *value)
{
    assert(list && "BlockList == null");
    if (list->count >= list->capacity) {
        long new_capacity = (list->capacity == 0) ? 4 : list->capacity * 2;
        void **new_data = (void**)realloc(list->data, sizeof(void*) * new_capacity);
        assert(new_data && "Failed to allocate memory for BlockList");
        list->data = new_data;
        list->capacity = new_capacity;
    }
    list->data[list->count++] = value;
}

inline void *blocklist_pop(BlockList *list)
{
    if (list->count == 0) return nullptr;
    return list->data[--list->count];
}

inline void blocklist_free_all(BlockList *list)
{
    if (!list->data) return;

    for (long i = 0; i < list->count; i++) {
        free(list->data[i]);
    }
    free(list->data);

    list->data = nullptr;
    list->count = 0;
    list->capacity = 0;
}



enum Allocator_Mode
{
    ALLOCATE,
    RESIZE,
    FREE,
    FREE_ALL
};

using Block_Allocator = void* (int, size_t, size_t, void*, void*, int);

struct Pool {
    size_t memblock_size = POOL_BUCKET_SIZE_DEFAULT;
    size_t alignment = 8;

    BlockList unused_memblocks;
    BlockList used_memblocks;
    BlockList obsoleted_memblocks;

    void *current_memblock = nullptr;
    void *current_pos = nullptr;
    size_t bytes_left = 0;

    Block_Allocator *block_allocator = nullptr;
    void *block_allocator_data = nullptr;
};

inline void pool_init(Pool *pool) {
    pool->current_memblock = nullptr;
    pool->current_pos = nullptr;
    pool->bytes_left = 0;
}

inline void *pool_alloc(Pool *pool, size_t size) {
    assert(pool != nullptr);

    // this version proves to be slighly more memory efficient since it considers the case when size is at perfect alignment and therefore no need to add any extra bytes
    size_t extra = (size % pool->alignment) ? pool->alignment - (size % pool->alignment) : 0;

    // size_t extra = pool->alignment - (size % pool->alignment); // how many bytes are we missing to alignment in size-to-be-allocated
    size += extra;

    // do we have enough bytes left in current block?
    ensure_memory_exists(pool, size);

    void *retval = pool->current_pos;
    pool->current_pos = (void*)((uintptr_t)pool->current_pos + size);
    pool->bytes_left -= size;

#ifdef _DEBUG
    totalNbyte += (int) size;
    printf("[POOL_ALLOC] extra=%zu\n", extra);
    printf("[POOL_ALLOC] %zu bytes %p\n", size, retval);
    printf("[POOL_ALLOC TOTAL SO FAR] %d bytes, %f KiB\n", totalNbyte, (float)totalNbyte/1024);
#endif

    return retval;
}

inline void ensure_memory_exists(Pool* pool, size_t size) {

    if (pool->bytes_left < size)
    {
        size_t bs = pool->memblock_size;

        while (bs < size)
            bs *= 2;

        if (bs > pool->memblock_size)
            resize_blocks(pool, bs);

        cycle_new_block(pool);
    }

    assert(pool->bytes_left >= size);
}

inline void resize_blocks(Pool *pool, size_t block_size)
{
    pool->memblock_size = block_size;

    if (pool->current_memblock)
        blocklist_push(&pool->obsoleted_memblocks, pool->current_memblock);

    for (long i = 0; i < pool->used_memblocks.count; i++)
        blocklist_push(&pool->obsoleted_memblocks, pool->used_memblocks.data[i]);

    pool->used_memblocks.count = 0;

    pool->current_memblock = nullptr;
}


inline void cycle_new_block(Pool *pool)
{
    if (pool->current_memblock)
        blocklist_push(&pool->used_memblocks, pool->current_memblock);

    void *new_block = nullptr;
    if (pool->unused_memblocks.count > 0) {
        new_block = blocklist_pop(&pool->unused_memblocks);
    } else {
        assert(pool->block_allocator != nullptr);
        new_block = pool->block_allocator(ALLOCATE, pool->memblock_size, 0, nullptr, pool->block_allocator_data, 0);

#ifdef _DEBUG
        printf("allocated NEW BLOCK in cycle_new_block\n");
#endif

    }

    pool->bytes_left = pool->memblock_size;
    pool->current_pos = new_block;
    pool->current_memblock = new_block;
}

inline void pool_reset(Pool* pool)
{
    if (pool->current_memblock) {
        blocklist_push(&pool->unused_memblocks, pool->current_memblock);
        pool->current_memblock = nullptr;
    }

    for (long i = 0; i < pool->used_memblocks.count; i++) {
        blocklist_push(&pool->unused_memblocks, pool->used_memblocks.data[i]);
    }
    pool->used_memblocks.count = 0;

    for (long i = 0; i < pool->obsoleted_memblocks.count; i++) {
        free(pool->obsoleted_memblocks.data[i]);
    }
    pool->obsoleted_memblocks.count = 0;

    pool->bytes_left = pool->memblock_size;
    pool->current_pos = pool->current_memblock;
}


inline void pool_release(Pool* pool)
{
    pool_reset(pool);

    if (pool->current_memblock) {
        free(pool->current_memblock);
        pool->current_memblock = nullptr;
    }

    blocklist_free_all(&pool->unused_memblocks);
    blocklist_free_all(&pool->used_memblocks);
    blocklist_free_all(&pool->obsoleted_memblocks);
}