#include <stdio.h>
#include <stdlib.h>

#define ROLL(d)	(launch(d) + launch(d) + launch(d))

typedef struct dice {
	int v, rolls;
}* Dice;

typedef struct game {
	int p1, p2, score1, score2;
}* Game;

int launch(Dice);
void updt(int*, int*, Dice);
int final1(Game);

main(int argc, char** argv) {
	Game g = (Game) calloc(1, sizeof(struct game));
	g -> p1 = 5;
	g -> p2 = 10;

	printf("%d\n", final1(g));
	
	return 0;
}


int launch(Dice d) {
	int r = (d -> v % 100) ? d -> v % 100 : 100;
	d -> rolls++;
	d -> v++;
	return r;
}


void updt(int* v, int* s, Dice d) {
	int r = ROLL(d);
	*v = (*v + r) % 10 ? (*v + r) % 10 : 10;
	*s += *v;
}

int final1(Game g) {
	Dice d = (Dice) malloc(sizeof(struct dice));
	d -> v = 1;
	d -> rolls = 0;
	
	printf("%d, %d\n", g -> p1, g -> p2);
	while (1) {
		updt(&(g -> p1), &(g -> score1), d);
		if (g -> score1 >= 1000) return d -> rolls * g -> score2;		
		updt(&(g -> p2), &(g -> score2), d);
		if (g -> score2 >= 1000) return d -> rolls * g -> score1;
	}
	return 0;
}
