#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define HEAD			3
#define TAIL			2
#define MAXSTR			100
#define MAXRLS			300
#define RULLEN			8
#define MAXL			27

#define head(x)			((x) ? (x -> in)  : 0)
#define tail(x)			((x) ? (x -> out) : 0) 
#define cmp(x, y)		(strcmp(head(x), head(y))) 
#define update_occs(c)		(occs[(c) - 'A']++)


typedef long lInt;

typedef struct rule {
	char in[HEAD];
	char out[TAIL];
	lInt old;
	lInt new;	// kinda useless but i didn't want to copy the array each time
}* RULE;


lInt occs[MAXL] = {0};

void populate(FILE*, RULE[], char[]);
void applyall_n(RULE[], char[], int);
void applyall(RULE[]);

int insert_rule(RULE[], int, RULE);
int find_r_ind(RULE[], char[]);
int min();
int max();

void rule_print(RULE);

main(int argc, char* argv[]) {
	FILE* f = fopen(argv[1], "r");
	RULE rules[MAXRLS];
	char str[MAXSTR];
	populate(f, rules, str);
	fclose(f);
	applyall_n(rules, str, atoi(argv[2]));
	printf("%ld\n", occs[max(occs)] - occs[min(occs)]);
	return 0;
}


void populate(FILE* f, RULE rs[], char s[]) {
	int len = 0;
	char c, str[RULLEN], hd[HEAD], tl[TAIL];
	RULE tmp;
            
	fscanf(f, "%s", s);
	
	while (fscanf(f, "%s -> %s", hd, tl) != EOF) {
		tmp = (RULE) calloc(1, sizeof(struct rule));
		strcpy(head(tmp), hd);
		strcpy(tail(tmp), tl);
		len = insert_rule(rs, len, tmp);
		if (len == -1) exit(1);
	}
	rs[len] = 0;
}

void applyall_n(RULE rs[], char str[], int n) {
	int i, ind;
	char two[3] = {0};
	for (i = 0; str[i]; i++)
		update_occs(str[i]);
	
	for (i = 1; str[i]; i++) {
		two[0] = str[i - 1];
		two[1] = str[i];
		ind = find_r_ind(rs, two);
		if (ind >= 0)
			(rs[ind] -> old)++;
	}
	
	while (n--)
		applyall(rs);
}

void applyall(RULE rs[]) {
	lInt old;
	char hd1[3] = {0}, hd2[3] = {0};
	int i, ind1, ind2;
	RULE r;
	for (i = 0; r = rs[i]; i++) {
		hd1[0] = head(r)[0];
		hd1[1] = tail(r)[0];
		hd2[0] = tail(r)[0];
		hd2[1] = head(r)[1];
		ind1 = find_r_ind(rs, hd1);
		ind2 = find_r_ind(rs, hd2);
		old = r -> old;
		if (ind1 >= 0)
			(rs[ind1] -> new) += old;
		if (ind2 >= 0)
			(rs[ind2] -> new) += old;
		occs[tail(r)[0] - 'A'] += old;
		r -> old = 0;
	}

	for (i = 0; rs[i]; i++) {
		rs[i] -> old = rs[i] -> new;
		rs[i] -> new = 0;
	}
}


// useless shit
int max() {
	lInt max = 0;
	int max_i = 0, i;
	for (i = 0; i < MAXL; i++)
		if (occs[i] > max) {
			max = occs[i];
			max_i = i;
		}
	return max_i;
}

int min() {
	lInt min = LONG_MAX;
	int min_i = 0, i;
	for (i = 0; i < MAXL; i++)
		if (occs[i] < min && occs[i]) {
			min = occs[i];
			min_i = i;
		}
	return min_i;
}

// gonna implement a binary search later, jkjk it's boring
int find_r_ind(RULE rs[], char s[]) {
	int i;
	for (i = 0; rs[i]; i++)
		if (!strcmp(head(rs[i]), s))
			return i;
       	return -1;
}

int insert_rule(RULE rs[], int len, RULE r) {
	if (len < MAXRLS) {
		rs[len] = r;
		len++;
		return len;
	}
	return -1;
}

// rappresentazione
void rule_print(RULE r) {
	printf("%s -> %s (%ld)\n", head(r), tail(r), r -> old);
}
