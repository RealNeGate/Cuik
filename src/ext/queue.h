#pragma once
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

typedef struct {
    void *data;
    size_t size;
    size_t item_size;
    size_t capacity;
    size_t start; 
} queue_t;

/* Creates a queue with the specified capacity and a size for each element. 
   Returns the queue created, or returns null if capacity or item_size is equal to 0. */
queue_t* queue_create(size_t capacity, size_t item_size) {
    if (capacity == 0 || item_size == 0) return NULL;
	
    queue_t *queue = malloc(sizeof(*queue));
    queue->data = malloc(capacity * item_size);
    queue->size = 0;
    queue->item_size = item_size;
    queue->capacity = queue->start = capacity;
    return queue;
}

/* Enqueue an item at the front of the queue. 
   Will block indefinitely if the queue is full. */
void queue_push(queue_t *queue, void *item) {
    while (queue->size == queue->capacity);
    --queue->start;
    memcpy(&((char*)queue->data)[queue->start * queue->item_size], item, queue->item_size);
    ++queue->size;
}

/* Dequeue an item at the back of the queue. 
   Does nothing if the queue is empty. */
void queue_pop(queue_t *queue, void *dest) {
    if (queue->size == 0) return;
    memcpy(dest, &((char*)queue->data)[queue->start * queue->item_size], queue->item_size);
    ++queue->start;
    --queue->size;
}

/* Free the queue back to the heap. */
void queue_free(queue_t *queue) {
    free(queue->data);
    free(queue);
}
