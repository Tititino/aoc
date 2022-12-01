#include <stdio.h>
#include <stdlib.h>

#define WORD	13

int final1();
int final2();

main(int argc, char* argv[]) {
	switch (*argv[1]) {
		case '1': printf("%d\n", final1()); break;
             	case '2': printf("%d\n", final2()); break;
                default : printf("dumbass");
	}
	return 0;
}

int final1() {
	int i, gamma = 0, epsilon, occs[2 * (WORD - 1)] = {0};
	char bin[WORD];
        while (scanf("%s", bin) != EOF)
		for (i = 0; bin[i]; i++)
			occs[(bin[i] - '0') ? i + WORD - 1 : i]++;
            
	for (i = 0; i < WORD - 1; i++) {
		// gamma = (gamma << 1) | (occs[i] > occs[i + WORD - 1]) ? 0 : 1; perché non funiona
		gamma <<= 1;
                gamma |= (occs[i] > occs[i + WORD - 1]) ? 0 : 1;
	}
	epsilon = (1 << i) - 1 & ~gamma;
        return gamma * epsilon;
}

int final2() {
	return 0;
}
