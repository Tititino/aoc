#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 	500
#define MAX_LINES 	500

#ifdef VERBOSE
#define PRINT_COND(string) 		printf("%s\n", (string))
#define PRINT_SEAFLOOR(seafloor) 	print_seafloor((seafloor))
#define PRINT_STEP(seafloor, count) 	printf("After %d step\n", (count)); \
					print_seafloor((seafloor)); 	    \
					printf("\n")
#else
#define PRINT_COND(string)
#define PRINT_SEAFLOOR(seafloor) 	
#define PRINT_STEP(seafloor, count) 	
#endif /* VERBOSE */

typedef struct seafloor {
	char** mat;
	size_t x;
	size_t y;
}* seafloor;

void print_seafloor(seafloor s);
int move(seafloor old, seafloor new, char type, int x, int y);
int am_i_free(seafloor s, int x, int y);
int move_all(seafloor* s, char type);

seafloor seafloor_from_file(FILE* input, const size_t max_x, const size_t max_y);
seafloor new_seafloor(const size_t x, const size_t y);
seafloor copy_seafloor(seafloor source);
void     del_seafloor(seafloor s);

int main(int argc, char* argv[]) {
	FILE *input = fopen(argv[1], "r");

	seafloor test = seafloor_from_file(input, MAX_LINE, MAX_LINES);
	
	PRINT_COND("Initial state:\n");
	PRINT_SEAFLOOR(test);

	int count = 0;
	int moves = 0;
	do {
		count++;
		moves = 0;
		moves += move_all(&test, '>');
		moves += move_all(&test, 'v');
		PRINT_STEP(test, count);
	} while (moves);

	PRINT_SEAFLOOR(test);
	printf("%d\n", count);

	del_seafloor(test);
	fclose(input);

	return 0;
}

seafloor seafloor_from_file(FILE* input, const size_t max_x, const size_t max_y) {
	char** mat = (char**) malloc(max_y * sizeof(char*));
	
	char* line = malloc(max_x * sizeof(char));
	size_t len_x = 0;
	size_t len_y;
	size_t i;

	for (i = 0; fgets(line, MAX_LINE, input); i++) {
		if (!len_x)
			len_x = strlen(line) - 1;
		char* temp = (char*) malloc(len_x * sizeof(char));
		// copy without the zero
		for (int k = 0; k < len_x; k++)
			temp[k] = line[k];
		mat[i] = temp;
	}

	len_y = i;

	mat = (char**) realloc(mat, len_y * sizeof(char*));

	seafloor seafloor_from_file = (seafloor) malloc(sizeof(struct seafloor));

	seafloor_from_file -> x   = len_x;
	seafloor_from_file -> y   = len_y;
	seafloor_from_file -> mat = mat;

	return seafloor_from_file;
}

void del_seafloor(seafloor s) {
	for (int i = 0; i < s -> y; i++) {
		free(s -> mat[i]);
	}
	free(s);
}

seafloor new_seafloor(const size_t x, const size_t y) {

	seafloor new_seafloor = (seafloor) malloc(sizeof(struct seafloor));
	new_seafloor -> x = x;
	new_seafloor -> y = y;

	new_seafloor -> mat = (char**) malloc(y * sizeof(char*));
	for (int i = 0; i < y; i++) {
		new_seafloor -> mat[i] = (char*) malloc(x * sizeof(char));
		for (int k = 0; k < x; k++)
			new_seafloor -> mat[i][k] = '.';
	}

	return new_seafloor;
}

seafloor copy_seafloor(seafloor source) {
	seafloor new_seafloor = (seafloor) malloc(sizeof(struct seafloor));
	new_seafloor -> x = source -> x;
	new_seafloor -> y = source -> y;

	new_seafloor -> mat = (char**) malloc(source -> y * sizeof(char*));
	for (int i = 0; i < source -> y; i++) {
		new_seafloor -> mat[i] = (char*) malloc(source -> x * sizeof(char));
		memcpy(new_seafloor -> mat[i], source -> mat[i], source -> x * sizeof(char));
	}

	return new_seafloor;
}

void print_seafloor(seafloor s) {
	printf("seafloor (y:%d, x:%d):\n", s -> y, s -> x);
	for (int y = 0; y < s -> y; y++) { 
		for (int x = 0; x < s -> x; x++) 
			printf("%c", s -> mat[y][x]);
		printf("\n");
	}
}

int move(seafloor old, seafloor new, char type, int x, int y) {
	int new_x;
	int new_y;
	
	if (old -> mat[y][x] != type) return 0;
	switch (type) {
		case '>':
			new_x = (x == (old -> x) - 1) ? 0 : x + 1;
			new_y = y;
			break;
		case'v':
			new_x = x;
			new_y = (y == (old -> y) - 1) ? 0 : y + 1;
			break;
		default:
			return 0;
	}
	if (am_i_free(old, new_x, new_y)) {
		new -> mat[new_y][new_x] = type;
		new -> mat[y][x] = '.';
		return 1;
	}
	return 0;
}

int am_i_free(seafloor s, int x, int y) {
	return s -> mat[y][x] == '.';
}

int move_all(seafloor* s, char type) {
	int acc = 0;
	seafloor new = copy_seafloor(*s);
	for (int y = 0; y < (*s) -> y; y++)
		for (int x = (*s) -> x - 1; x >= 0; x--)
			acc += move(*s, new, type, x, y);
	del_seafloor(*s);
	*s = new;
	return acc;
}

