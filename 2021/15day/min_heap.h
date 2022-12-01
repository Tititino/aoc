#ifndef MIN_HEAP_H
#define MIN_HEAP_H

#ifndef MAXHEAP
#define MAXHEAP	1000
#endif

typedef struct heapnode {
	int key;
	int val;
}* Node;

typedef struct heap {
	int size;
	Node* pvect;
}* Heap;

Heap mheap_new();

int mheap_isempty(Heap);
int mheap_findMin(Heap);
int mheap_deleteMin(Heap);
void mheap_insert(Heap, int, int);
void mheap_delete(Heap, int);
void mheap_changeKey(Heap, int, int);

void mheap_destroy();

void mheap_print(Heap);

void fixHeap(Heap, int);

#endif
