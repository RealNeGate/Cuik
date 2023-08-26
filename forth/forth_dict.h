#include <hashes.h>

#define DICT_API static

////////////////////////////////
// Word pool
////////////////////////////////
typedef int WordIndex;

// word index -> word
static struct {
    size_t cap;
    Word* entries;
    uint64_t* allocated;
} words;

static void init_word_pool(void) {
    words.cap = 1024;

    size_t word_cap = (words.cap + 63) / 64;
    words.entries = cuik_malloc(words.cap * sizeof(Word));
    words.allocated = cuik_calloc(word_cap, sizeof(uint64_t));
}

static WordIndex alloc_word(void) {
    size_t word_cap = (words.cap + 63) / 64;
    for (size_t i = 0; i < word_cap; i++) {
        if (words.allocated[i] == UINT64_MAX) continue;

        // find some unallocated slot
        int index = words.allocated[i] ? __builtin_ffsll(~words.allocated[i]) - 1 : 0;

        words.allocated[i] |= (1ull << index);
        return i*64 + index;
    }

    assert(0 && "work pool full");
    return -1;
}

static void ref_word(WordIndex w) {
    assert(w >= 0 && w < words.cap);
    atomic_fetch_add(&words.entries[w].refs, 1);
}

static void unref_word(WordIndex w) {
    assert(w >= 0 && w < words.cap);
    if (atomic_fetch_sub(&words.entries[w].refs, 1) == 1) {
        // actually free
        assert(is_main_thread && "word release must happen on the main to avoid needing a lock");
        words.allocated[w / 64] &= ~(1ull << (w % 64));
    }
}

////////////////////////////////
// Dictionary
////////////////////////////////
// length segregated entry (<= 4)
typedef struct {
    uint32_t key;
    WordIndex val;
} DictionaryEntry4;

typedef struct {
    const char* key;
    WordIndex val;
} DictionaryEntry;

typedef struct Field Field;
struct Field {
    Field* next;
    const char* name;
    ptrdiff_t offset;
};

struct Dictionary {
    Dictionary* parent;
    const char* name;

    // string -> word index
    struct {
        uint32_t exp, count;
        DictionaryEntry4* entries;
    } short_table;

    struct {
        uint32_t exp, count;
        DictionaryEntry* entries;
    } big_table;

    size_t field_count, data_size;
    Field* fields; // in reverse

    // dictionaries have variables which take up some space.
    // they'll also store relevant some strings in here.
    TB_Arena data;
};

static bool dict_entry4_eq(uint32_t a, uint32_t b, size_t len) {
    uint64_t cmp_mask = ((1ull << (len * 8)) - 1ull);
    return ((a ^ b) & cmp_mask) == 0;
}

static size_t dict_entry4_lookup(Dictionary* dict, size_t len, uint32_t key) {
    ASSUME(len <= 4); // i know clang wouldn't actually care... but it should
    if (dict->short_table.entries == NULL) {
        return SIZE_MAX;
    }

    uint32_t h = tb__murmur3_32(&key, len);

    size_t mask = (1 << dict->short_table.exp) - 1;
    size_t first = h & mask, i = first;
    size_t high_bit = ~(SIZE_MAX >> ((size_t) 1));

    do {
        uint32_t hm_key = dict->short_table.entries[i].key;
        if (hm_key == 0) return i;
        if (dict_entry4_eq(hm_key, key, len)) return high_bit | i;
        i = (i + 1) & mask;
    } while (i != first);

    return SIZE_MAX;
}

static bool dict_entry_big_eq(const char* a, const char* b, size_t len) {
    return strlen(a) == len && memcmp(a, b, len) == 0;
}

static size_t dict_entry_big_lookup(Dictionary* dict, size_t len, const char* key) {
    if (dict->big_table.entries == NULL) {
        return SIZE_MAX;
    }

    uint32_t h = tb__murmur3_32(key, len);

    size_t mask = (1 << dict->big_table.exp) - 1;
    size_t first = h & mask, i = first;
    size_t high_bit = ~(SIZE_MAX >> ((size_t) 1));

    do {
        const char* hm_key = dict->big_table.entries[i].key;
        if (hm_key == 0) return i;
        if (dict_entry_big_eq(hm_key, key, len)) return high_bit | i;
        i = (i + 1) & mask;
    } while (i != first);

    return SIZE_MAX;
}

// 0 for not found
DICT_API WordIndex dict_get(Dictionary* dict, ptrdiff_t len, const char* key) {
    if (len < 0) len = strlen(key);

    do {
        size_t high_bit = ~(SIZE_MAX >> ((size_t) 1));
        if (len <= 4) {
            uint32_t short_key = 0;
            memcpy(&short_key, key, len);

            size_t index = dict_entry4_lookup(dict, len, short_key);
            if (index != SIZE_MAX && index & high_bit) {
                return dict->short_table.entries[index & ~high_bit].val;
            }
        } else {
            size_t index = dict_entry_big_lookup(dict, len, key);
            if (index != SIZE_MAX && index & high_bit) {
                return dict->big_table.entries[index & ~high_bit].val;
            }
        }

        dict = dict->parent;
    } while (dict != NULL);

    return 0;
}

static size_t try_rehash(size_t index, size_t count, size_t exp) {
    size_t cap = 1ull << exp;
    return count >= ((cap * 3) / 4) ? SIZE_MAX : index;
}

static void dict_entry4_put(Dictionary* dict, ptrdiff_t len, uint32_t short_key, WordIndex val) {
    size_t high_bit = ~(SIZE_MAX >> ((size_t) 1));
    size_t index = try_rehash(dict_entry4_lookup(dict, len, short_key), dict->short_table.count, dict->short_table.exp);

    // rehash
    while (index == SIZE_MAX) {
        size_t new_exp = dict->short_table.exp == 0 ? 6 : dict->short_table.exp + 1;
        size_t old_cap = 1ull << dict->short_table.exp;

        DictionaryEntry4* old = dict->short_table.entries;
        dict->short_table.entries = cuik_calloc((1ull << new_exp), sizeof(DictionaryEntry4));
        dict->short_table.exp = new_exp;

        if (old != NULL) {
            for (size_t i = 0; i < old_cap; i++) {
                DictionaryEntry4 e = old[i];
                if (e.key != 0) dict_entry4_put(dict, len, short_key, e.val);
            }
            cuik_free(old);
        }

        index = dict_entry4_lookup(dict, len, short_key);
    }

    dict->short_table.count++;
    dict->short_table.entries[index & ~high_bit].key = short_key;
    dict->short_table.entries[index & ~high_bit].val = val;
}

static void dict_entry_big_put(Dictionary* dict, ptrdiff_t len, const char* key, WordIndex val) {
    size_t high_bit = ~(SIZE_MAX >> ((size_t) 1));
    size_t index = try_rehash(dict_entry_big_lookup(dict, len, key), dict->big_table.count, dict->big_table.exp);

    while (index == SIZE_MAX) {
        size_t new_exp = dict->big_table.exp == 0 ? 6 : dict->big_table.exp + 1;
        size_t old_cap = 1ull << dict->big_table.exp;

        DictionaryEntry* old = dict->big_table.entries;
        dict->big_table.entries = cuik_calloc((1ull << new_exp), sizeof(DictionaryEntry));
        dict->big_table.exp = new_exp;

        if (old != NULL) {
            for (size_t i = 0; i < old_cap; i++) {
                DictionaryEntry e = old[i];
                if (e.key != NULL) dict_entry_big_put(dict, len, key, e.val);
            }
            cuik_free(old);
        }


        index = dict_entry_big_lookup(dict, len, key);
    }

    dict->big_table.count++;
    dict->big_table.entries[index & ~high_bit].key = key;
    dict->big_table.entries[index & ~high_bit].val = val;
}

DICT_API void dict_put(Dictionary* dict, ptrdiff_t len, const char* key, WordIndex val) {
    if (len < 0) len = strlen(key);

    if (len <= 4) {
        uint32_t short_key = 0;
        memcpy(&short_key, key, len);
        dict_entry4_put(dict, len, short_key, val);
    } else {
        dict_entry_big_put(dict, len, key, val);
    }
}

static void dict__migrate_word(Word* w, Dictionary* dict, Dictionary* old_dict) {
    if (w->tag == WORD_VAR) {
        // if no old version existed, don't migrate
        WordIndex found = dict_get(old_dict, w->name.length, (const char*) w->name.data);
        if (found < 0 && words.entries[-1000 - found].tag == WORD_VAR) {
            void* old_ptr = (void*) words.entries[-1000 - found].ops[0];
            void* new_ptr = (void*) w->ops[0];
            memcpy(new_ptr, old_ptr, sizeof(int64_t));

            // log_debug("dict: migrate %s (%p -> %p)", w->name.data, old_ptr, new_ptr);
        }
    }
}

// migrates any data into a new arena
void dict_migrate(Dictionary* dict, Dictionary* old_dict) {
    log_debug("dict: migrating old variables to new dictionary");

    if (dict->short_table.entries != NULL) {
        size_t short_cap = 1ull << dict->short_table.exp;
        for (size_t i = 0; i < short_cap; i++) {
            if (dict->short_table.entries[i].key != 0 && dict->short_table.entries[i].val < 0) {
                dict__migrate_word(&words.entries[-1000 - dict->short_table.entries[i].val], dict, old_dict);
            }
        }
    }

    if (dict->big_table.entries != NULL) {
        size_t big_cap = 1ull << dict->big_table.exp;
        for (size_t i = 0; i < big_cap; i++) {
            if (dict->big_table.entries[i].key != NULL && dict->big_table.entries[i].val < 0) {
                dict__migrate_word(&words.entries[-1000 - dict->big_table.entries[i].val], dict, old_dict);
            }
        }
    }
}

void dict_free(Dictionary* dict) {
    // release all words
    if (dict->short_table.entries != NULL) {
        size_t short_cap = 1ull << dict->short_table.exp;
        for (size_t i = 0; i < short_cap; i++) {
            if (dict->short_table.entries[i].key != 0 && dict->short_table.entries[i].val < 0) {
                unref_word(-1000 - dict->short_table.entries[i].val);
                dict->short_table.entries[i].key = 0;
            }
        }
    }

    if (dict->big_table.entries != NULL) {
        size_t big_cap = 1ull << dict->big_table.exp;
        for (size_t i = 0; i < big_cap; i++) {
            if (dict->big_table.entries[i].key != NULL && dict->big_table.entries[i].val < 0) {
                unref_word(-1000 - dict->big_table.entries[i].val);
                dict->big_table.entries[i].key = NULL;
            }
        }
    }

    tb_arena_clear(&dict->data);
}
