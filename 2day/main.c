#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define WORD 	50

int final1();
int final2();

main(int argc, char *argv[]) {
	switch(*argv[1]) {
        	case '1': printf("%d\n",  final1()); break;
                case '2': printf("%d\n",  final2()); break;
                default: printf("dumbfuck\n"); break;
       	}
        return 0;
}

int final1() {
	char c[WORD];
	int h = 0, d = 0, n;

        while (scanf("%s %d", c, &n) != EOF)
		switch (*c) {
			case 'f': h += n; break;
                        case 'd': d += n; break;
                        case 'u': d -= n; break;
                }
       	return d * h;
}

int final2() {
	char c[WORD];
        int d = 0, h = 0, a = 0, n;

        while (scanf("%s %d", c, &n) != EOF)
            	switch (*c) {
                        case 'f': h += n; d += (a * n); break;
                        case 'd': a += n; break;
                        case 'u': a -= n; break;
               	}
    	return d * h;
}
