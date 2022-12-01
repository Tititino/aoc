#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
/* #include "min_heap.h" */

#define rows(x)	((x) -> n)
#define cols(x)	((x) -> m)
#define mat(x)	((x) -> mat)

/* typedef Heap PQueue; */

typedef struct mat {
	int n, m;
	int** mat;
}* Mat;

int getn(char[]);
int getm(char[]);
Mat populate(char[]);
int path_find(Mat, int, int, int, int);

void mat_print(Mat);

main(int argc, char* argv[]) {
	Mat mat = populate(argv[1]);
	printf("ciao\n");
	int i = path_find(mat, 0, 0, cols(mat) - 1, rows(mat) - 1);
	printf("%d\n", i);
	return 0;
}


int getn(char file[]) {
           	FILE* f = fopen(file, "r");
           	int i;
	char c;
         	for (i = 0; ; i++) {
		while ((c = getc(f)) != '\n' && c != EOF);
    		if (c == EOF) break;
           	}
	fclose(f);
	return i;
}

int getm(char file[]) {
	FILE* f = fopen(file, "r");
	int i = 0;
	while (getc(f) != '\n') i++;
	fclose(f);
	return i;
}

Mat populate(char file[]) {
	int n = getn(file), m = getm(file), y, x;
	Mat matrix = (Mat) malloc(sizeof(struct mat));
	rows(matrix) = n;
	cols(matrix) = m;
	mat(matrix)  = (int**) malloc(n * sizeof(int*));
	for (y = 0; y < rows(matrix); y++)
		mat(matrix)[y] = (int*) malloc(m * sizeof(int));

	FILE* f = fopen(file, "r");
	for (y = 0; y < rows(matrix); y++) {
		for (x = 0; x < cols(matrix); x++)
			mat(matrix)[y][x] = getc(f) - '0';
		getc(f);
	}
	return matrix;
}

int path_find(Mat mat, int xinizio, int yinizio, int xfine, int yfine) {
	int x, y, k, res = 0;;
	int** d = (int**) malloc(rows(mat) * sizeof(int*));
	for (y = 0; y < cols(mat); y++)
		d[y] = (int*) malloc(cols(mat) * sizeof(int));

	for (y = 0; y < rows(mat); y++)
		for (x = 0; x < cols(mat); x++)
			if (x != xinizio && y != yinizio)
				d[y][x] = INT_MAX;
	printf("ciao");
	for (k = 1; k < rows(mat) * cols(mat); k++) {
		for (y = 0; y < rows(mat); y++) {
			for (x = 0; x < cols(mat); x++) {
				if (x > 0)
					if (d[y][x] != INT_MAX && d[y][x] + mat(mat)[y][x - 1] < d[y][x - 1])
						d[y][x - 1] = d[y][x] + mat(mat)[y][x - 1];
				if (x < cols(mat) - 1)
					if (d[y][x] != INT_MAX && d[y][x] + mat(mat)[y][x + 1] < d[y][x + 1])
						d[y][x + 1] = d[y][x] + mat(mat)[y][x + 1];
				if (y > 0)
					if (d[y][x] != INT_MAX && d[y][x] + mat(mat)[y - 1][x] < d[y - 1][x])
						d[y - 1][x] = d[y][x] + mat(mat)[y - 1][x];
				if (x < rows(mat) - 1)
					if (d[y][x] != INT_MAX && d[y][x] + mat(mat)[y + 1][x] < d[y + 1][x])
						d[y + 1][x] = d[y][x] + mat(mat)[y + 1][x];
			}
		}
	}
	res = d[yfine][xfine];
	free(d);
	return res;
}

/* int djikstra(Mat mat, int xinizio, int yinizio, int xfine, int yfine) {
/* 	printf("ciao"); */
/* 	int x = 0, y = 0, min, minx_i, miny_i, i, res; */
/* 	int** d = (int**) malloc(rows(mat) * sizeof(int*)); // i could do this with just an array but i don't really have the mental energy to */
/* 	int** C = (int**) malloc(rows(mat) * sizeof(int*)); */
/* 	for (i = 0; i < cols(mat); i++) { */
/* 		d[i] = (int*) malloc(cols(mat) * sizeof(int)); */
/* 		C[i] = (int*) malloc(cols(mat) * sizeof(int)); */
/*         	} */
	
/* 	d[yinizio][xinizio] = 0; */
/* 	for (y = 0; y < rows(mat); y++) */
/* 		for (x = 0; x < cols(mat); x++) */
/* 			if (x != xinizio && y != yinizio) */
/* 				d[y][x] = -1; */
	
/* 	for (y = 0; y < rows(mat); y++) */
/* 		for (x = 0; x < cols(mat); x++) */
/* 			C[y][x] = mat(mat)[y][x]; */
/* 	while (1) { */
/* 	/\* 	for (y = 0, min = INT_MAX, miny_i = 0; y < rows(mat); y++) *\/ */
/* 	/\* 		for (x = 0, minx_i = 0; y < cols(mat); x++) *\/ */
/* 	/\* 			if (mat(mat)[y][x] < min) { *\/ */
/* 	/\* 				min = mat(mat)[y][x]; *\/ */
/* 	/\* 				minx_i = x; *\/ */
/* 	/\* 				miny_i = y; *\/ */
/* 	/\* 			 } *\/ */
			       
/* 	/\* 	x = minx_i; *\/ */
/* 	/\* 	y = miny_i; *\/ */
/* 	/\* 	C[y][x] = INT_MAX; *\/ */
		
/* 	/\* 	if (x == xfine && y == yfine) *\/ */
/* 	/\* 		break; *\/ */

/* 	/\* 	if (x != 0) *\/ */
/* 	/\* 		if (d[y][x] + mat(mat)[y][x - 1] < d[y][x - 1]) *\/ */
/* 	/\* 			d[y][x - 1] = d[y][x] + mat(mat)[y][x - 1]; *\/ */
/* 	/\* 	if (x != cols(mat) - 1) *\/ */
/* 	/\* 		if (d[y][x] + mat(mat)[y][x + 1] < d[y][x + 1]) *\/ */
/* 	/\* 			d[y][x + 1] = d[y][x] + mat(mat)[y][x + 1]; *\/ */
/* 	/\* 	if (y != 0) *\/ */
/* 	/\* 		if (d[y][x] + mat(mat)[y - 1][x] < d[y - 1][x]) *\/ */
/* 	/\* 			d[y - 1][x] = d[y][x] + mat(mat)[y - 1][x]; *\/ */
/* 	/\* 	if (x != rows(mat) - 1) *\/ */
/* 	/\* 		if (d[y][x] + mat(mat)[y + 1][x] < d[y + 1][x]) *\/ */
/* 	/\* 			d[y + 1][x] = d[y][x] + mat(mat)[y + 1][x]; *\/ */
/* 		break; */
/* 	} */
/* 	res = d[y][x]; */
/* 	/\* for (i = 0; i < rows(mat); i++) { *\/ */
/* 	/\* 	free(d[i]); *\/ */
/* 	/\* 	free(C[i]); *\/ */
/* 	/\* } *\/ */
/* 	/\* /\\* free(C); *\\/ *\/ */
/* 	/\* free(d); *\/ */
/* 	return res; */
/* 	return 0; */
/* }	 */


void mat_print(Mat m) {
	int y, x;
	for (y = 0; y < rows(m); y++) {
		for (x = 0; x < cols(m); x++)
			printf("%d ", mat(m)[y][x]);
		printf("\n");
           	}
}
