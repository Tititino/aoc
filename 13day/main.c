#include <stdio.h>
#include <stdlib.h>

#define MAX		5000

enum {X, Y};

typedef struct instr {
	int type, val;
}* Instr;

typedef struct point {
	int x, y;
}* Point;


void populate(FILE*, Instr[], int***, int*, int*);

void exec(Instr[], int**, int*, int*);
void exec_one(Instr, int**, int*, int*);
void foldx(int**, int*, int*, int);
void foldy(int**, int*, int*, int);
int count(int**, int, int);

void instr_print(Instr);
void point_print(Point);
void mat_print(int**, int, int);


main(int argc, char* argv[]) {
	FILE* f = fopen(argv[1], "r");
	Instr instructions[MAX];
	int** mat;
	int x, y;
	populate(f, instructions, &mat, &x, &y);
	fclose(f);
	switch (*argv[2]) {
		case '1':
			exec_one(instructions[0], mat, &x, &y); 
			printf("%d\n", count(mat, x, y));
			break;
		case '2':
			exec(instructions, mat, &x, &y);
			mat_print(mat, x, y);
			break;
	}
	return 0;
}

void instr_print(Instr i) {
	printf("%c: %d", i -> type ? 'y' : 'x', i -> val);
}

void point_print(Point p) {
	printf("<%d, %d>", p -> x, p -> y);
}

void mat_print(int** mat, int x, int y) {
	int ix, iy;
	for (iy = 0; iy < y; iy++) {
		for (ix = 0; ix < x; ix++)
			printf("%c", mat[iy][ix] ? '#' : '.');
		printf("\n");
	}
}


void populate(FILE* f, Instr in[], int*** mat, int* maxx, int* maxy) {
	Point points[MAX], p;
	Instr new_instr;
	int x, y, a = 0, b = 0, i = 0, ins = 0;
	char str[MAX], c;

	while (fscanf(f, "%d,%d", &x, &y) && i < MAX) {
		a = (x > a) ? x : a;
		b = (y > b) ? y : b;
		p = (Point) malloc(sizeof(struct point));
		p -> x = x;
		p -> y = y;
		points[i++] = p;
	}
	points[i] = 0;
	
	a++;
	b++;
	*maxx = a;
	*maxy = b;
	*mat = (int**) calloc(b, sizeof(int*));
	for (i = 0; i < b; i++)
		(*mat)[i] = (int*) calloc(a, sizeof(int));
            
	for (i = 0; p = points[i]; i++) {
		(*mat)[p -> y][p -> x] = 1;
		free(p);
	}

	while (fscanf(f, "%s", str) != EOF) {
		for (i = 0; c = str[i]; i++) {
			switch (c) {
				case 'x':
					new_instr = (Instr) malloc(sizeof(struct instr));
					new_instr -> type = X;
					new_instr -> val = atoi(str + i + 2);
					in[ins++] = new_instr;
					break;
				case 'y':
					new_instr = (Instr) malloc(sizeof(struct instr));
					new_instr -> type = Y;
					new_instr -> val = atoi(str + i + 2);
					in[ins++] = new_instr;
					break;
			}
		}
	}
	in[ins] = 0;
}

void exec(Instr in[], int** mat, int* x, int* y) {
	int i;
	Instr ins;
	for (i = 0; ins = in[i]; i++) {
		if (ins -> type)
			foldy(mat, x, y, ins -> val);
		else
			foldx(mat, x, y, ins -> val);
		free(ins);
	}
}

void exec_one(Instr i, int** mat, int* x, int* y) {
	if (i -> type)
		foldy(mat, x, y, i -> val);
	else
		foldx(mat, x, y, i -> val);
}

void foldx(int** mat, int* x, int* y, int n) {
	int ix, iy;
	for (iy = 0; iy < *y; iy++)
		for (ix = 1; n + ix < *x && n - ix >= 0; ix++)
			mat[iy][n - ix] |= mat[iy][n + ix];
	*x = n;
}

void foldy(int** mat, int* x, int* y, int n) {
	int ix, iy;
	for (ix = 0; ix < *x; ix++)
		for (iy = 1; n + iy < *y && n - iy >= 0; iy++)
			mat[n - iy][ix] |= mat[n + iy][ix];
	*y = n;
}

int count(int** mat, int x, int y) {
	int acc = 0, i;
	for (; y; y--)
		for (i = 0; i < x; i++)
			if (mat[y - 1][i]) acc++;
	return acc;	
}
