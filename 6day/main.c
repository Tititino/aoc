#include <stdio.h>
#include <stdlib.h>

#define LEN1	9

typedef long long int intType;

intType sum(intType*, int);
intType sx_shift(intType*, int);
intType final1(int);
int final2(int);

main(int argc, char* argv[]) {
	switch(*argv[1]) {
		case '1': printf("%ld\n", final1(atoi(argv[2]))); break;
		// case '2': printf("%d\n", final2(atoi(argv[2]))); break;
		default: printf("dumbdass\n"); break;
	}
	return 0;
}

intType sum(intType* arr, int len) {
	intType acc, i;
	for (i = acc = 0; i < len; i++)
		acc += arr[i];
	return acc;
}

intType sx_shift(intType* arr, int len) {
	long int fst = arr[0];
	int i;
	for (i = 1; i < len; i++)
		arr[i - 1] = arr[i];
	arr[i - 1] = fst;
	arr[6] += fst;
	return fst;
}

intType final1(int days) {
	intType fish[LEN1] = {0};
	char c;
	// populate the ogs array
	while ((c = getchar()) != EOF) {
		fish[c - '0']++;
		getchar();		 // burns the comma
	}
	while (days--)
	sx_shift(fish, LEN1);
	return sum(fish, LEN1);
}

// int final2(int days) {
// 	return 0;
// }
