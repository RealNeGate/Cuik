
void PRIO_FN(insert)(PRIO_TYPE* pq, PRIO_ELEM t) {
    pq->data[pq->count++] = t;

    // Shift up
    ptrdiff_t j = pq->count - 1;
    while (0 <= j && j < pq->count) {
        ptrdiff_t i = (j-1)/2;
        if (i == j || PRIO_FN(cmp)(pq->data[j], pq->data[i]) >= 0) {
            break;
        }

        PRIO_ELEM tmp = pq->data[i];
        pq->data[i] = pq->data[j];
        pq->data[j] = tmp;
        j = i;
    }
}

PRIO_ELEM PRIO_FN(peek)(PRIO_TYPE* pq) {
    if (pq->count == 0) {
        return NULL;
    }
    return pq->data[0];
}

PRIO_ELEM PRIO_FN(pop)(PRIO_TYPE* pq) {
    if (pq->count == 0) {
        return NULL;
    }

    ptrdiff_t n = pq->count - 1;
    {
        PRIO_ELEM tmp = pq->data[0];
        pq->data[0] = pq->data[n];
        pq->data[n] = tmp;
    }

    // O(n log n)
    ptrdiff_t i = 0;
    if (0 <= i && i <= n) {
        for (;;) {
            ptrdiff_t j1 = 2*i + 1;
            if (0 > j1 || j1 >= n) break;
            ptrdiff_t j = j1;
            ptrdiff_t j2 = j1 + 1;
            if (j2 < n && PRIO_FN(cmp)(pq->data[j2], pq->data[j1]) < 0) {
                j = j2;
            }
            if (PRIO_FN(cmp)(pq->data[j], pq->data[i]) >= 0) {
                break;
            }

            PRIO_ELEM tmp = pq->data[i];
            pq->data[i] = pq->data[j];
            pq->data[j] = tmp;
            i = j;
        }
    }
    return pq->data[--pq->count];
}
#undef PRIO_ELEM
#undef PRIO_TYPE
#undef PRIO_FN
