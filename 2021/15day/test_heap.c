#include <stdio.h>
#include <stdlib.h>
#include "min_heap.h"

main() {
	Heap h = mheap_from_arr(int[]{10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0});
	
	for (int i = 0; i < h -> size; i++) {
		fixHeap(h, i);
	}
	return 0;
}
