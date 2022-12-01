#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define size(x)	((x) -> size)
#define key(x)	((x) -> key)
#define val(x)	((x) -> val)

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
Heap mheap_from_arr(int[]);

int mheap_isempty(Heap);
int mheap_findMin(Heap);
int mheap_deleteMin(Heap);
void mheap_insert(Heap, int, int);
void mheap_delete(Heap, int);
void mheap_changeKey(Heap, int, int);

void mheap_destroy(Heap);

void mheap_printf(Heap);

void fixHeap(Heap, int);

int mheap_isempty(Heap h) {
	return !size(h);
}

Heap mheap_new() {
	Heap h = (Heap) calloc(1, sizeof(struct heap));
	h -> pvect = (Node*) calloc(MAXHEAP, sizeof(Node));
	return h;
}

int mheap_findMin(Heap h) {
	if (size(h)) return val((h -> pvect)[0]);
	else exit(1);
}

int mheap_deleteMin(Heap h) {
	int res = val((h -> pvect)[0]);
	mheap_delete(h, val((h -> pvect)[0]));

	return res;
}

void mheap_insert(Heap h, int e, int k) {
	Node n = (Node) malloc(sizeof(struct heapnode));
	n -> key = k;
	n -> val = e;
	
           	(h -> pvect)[size(h)++] = n;
	fixHeap(h, 0);
}

void mheap_delete(Heap h, int e) {
	int i;
	for (i = 0; i < size(h); i++) {
		if (val((h -> pvect)[i]) == e) {
			key((h -> pvect)[i]) = INT_MAX;
			break;
		}
	}
	fixHeap(h, 0);
	
	free((h -> pvect)[size(h)++]);
	(h -> pvect)[size(h)] = 0;
	size(h)--;
}

void mheap_changeKey(Heap h, int e, int k) {
	int i;
	for (i = 0; i < size(h); i++) {
		if (val((h -> pvect)[i]) == e) {
			key((h -> pvect)[i]) = k;
			break;
		}
	}
	fixHeap(h, 0);
}


void fixHeap(Heap h, int r) {
	int v = r, x = key((h -> pvect)[v]), flag = 1, u;
	do {
		if (2 * v + 2 >= size(h))
			flag = 0;
		else {
			u = (key((h -> pvect)[2 * v + 1]) < key((h -> pvect)[2 * v + 2])) ? 2 * v + 1 : 2 * v + 2;
			if (key((h -> pvect)[u]) < x) {
				key((h -> pvect)[v]) = key((h -> pvect)[u]);
				v = u;
			}
			else
				flag = 0;
	      	}
	} while (flag);
	key((h -> pvect)[v]) = x;
}


/* void fixHeap(Heap h, int i) { */
/* 	int l = 2 * i + 1; */
/* 	int r = 2 * i + 2; */
/* 	Node temp; */
/* 	int smallest = i; */
/* 	if (l < size(h) && key((h -> pvect)[l]) < key((h -> pvect)[i])) */
/* 		smallest = l; */
/*            	if (r < size(h) && key((h -> pvect)[r]) < key((h -> pvect)[smallest])) */
/* 		smallest = r; */
/* 	if (smallest != i) { */
/* 		temp = (h -> pvect)[i]; */
/* 		(h -> pvect)[i] = (h -> pvect)[smallest]; */
/* 		(h -> pvect)[smallest] = temp; */
/* 		fixHeap(h, smallest); */
/* 	} */
/* } */

void mheap_destroy(Heap h) {
	int i;
	// i'll use MAXHEAP and not size(h) because i don't really trust myself
	for (i = 0; i < MAXHEAP; i++)
		free((h -> pvect)[i]);

	free((h -> pvect));
	free(h);
}


void mheap_print(Heap h) {
	int i;
	for (i = 0; i < size(h); i++)
		printf("%d ", val((h -> pvect)[i]));
	printf("\n");
}	
