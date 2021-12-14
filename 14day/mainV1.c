#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define HEAD		3
#define TAIL		2
#define MAXSTR		1000000
#define MAXRLS		300
#define RULLEN		8
#define MAXL		27

#define head(x)		((x) ? (x -> in)  : 0)
#define tail(x)		((x) ? (x -> out) : 0) 
#define cmp(x, y)		(strcmp(head(x), head(y))) 
#define update_occs(c)		(occs[(c) - 'A']++)


typedef struct rule {
	char in[HEAD];
	char out[TAIL];
}* RULE;


typedef long lInt;


lInt occs[MAXL] = {0};


int populate(FILE*, RULE[], char[]);
int applyall_n(RULE[], char[], int);
int applyall(RULE[], char[]);
short* iterate(char[]);

int take_two(char[], int, char[]);
int insert_rule(RULE[], int, RULE);
int find_r_ind(RULE[], RULE);
int _find_r_ind(RULE[], char[]);
int shift_dx(RULE[], int, int*);
int shift_s_dx(char[], int);
int min();
int max();

void rule_print(RULE);

main(int argc, char* argv[]) {
	FILE* f = fopen(argv[1], "r");
	RULE rules[MAXRLS];
	char str[MAXSTR];
	int len = populate(f, rules, str);
	fclose(f);
	applyall_n(rules, str, atoi(argv[2]));
	printf("%ld\n", occs[max(occs)] - occs[min(occs)]);
	return 0;
}


int populate(FILE* f, RULE rs[], char s[]) {
	int len = 0, n;
	size_t strlen;
	char c, str[RULLEN], hd[HEAD], tl[TAIL];
	RULE tmp;
	
	
	fscanf(f, "%s", s);
	
	while (1) {
		// magari riprovo con scanf
		while ((c = getc(f)) == ' ' || c == '\n');
		if (c == EOF) break;
		hd[0] = c;
		hd[1] = (c = getc(f));
		hd[2] = 0;
		while ((c = getc(f)) == ' ' || c == '-' || c == '>');
		tl[0] = c;
		tl[1] = 0;
		tmp = (RULE) malloc(sizeof(struct rule));
		strcpy(head(tmp), hd);
		strcpy(tail(tmp), tl);
		len = insert_rule(rs, len, tmp);
		if (len == -1) exit(1);
	}
	rs[len] = 0;
	return len;
}

int applyall_n(RULE rs[], char str[], int n) {
	int i;
	for (i = 0; str[i]; i++)
		update_occs(str[i]);
	while (n--) {
		applyall(rs, str);
	}
}

int applyall(RULE rs[], char str[]) {
	char two[3], c;
	int i, ind;
	
	for (i = 1; str[i]; i++) {
		two[0] = str[i - 1];
		two[1] = str[i];
		two[2] = 0;
		ind = _find_r_ind(rs, two);
		if (ind >= 0) {
			shift_s_dx(str, i);			
			c = tail(rs[ind])[0];
			str[i++] = c;
			update_occs(c);
		}
	}
	return i;
}

short* iterate(char str[]) {
	short* letts = (short*) calloc(MAXL, sizeof(short)), i;
	for (i = 0; str[i]; i++)
		letts[str[i] - 'A']++;
	return letts;
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

int shift_s_dx(char s[], int from) {
	char prec, now;
	int i;
	prec = s[from];
	for (i = from + 1; i < MAXSTR && prec; i++) {
		now = s[i];
		s[i] = prec;
		prec = now;
	}
}

int shift_dx(RULE rs[], int from, int *len) {
	int i;
	for (i = *len; i > from; i--) {
		rs[i] = rs[i - 1];
	}
	(*len)++;
	return 1;
}

// gonna implement a binary search later
int _find_r_ind(RULE rs[], char s[]) {
	int i;
	for (i = 0; rs[i]; i++)
		if (!strcmp(head(rs[i]), s))
			return i;
       	return -1;
}

int find_r_ind(RULE rs[], RULE r) {
	return _find_r_ind(rs, head(r));
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
	printf("%s -> %s\n", head(r), tail(r));
}
