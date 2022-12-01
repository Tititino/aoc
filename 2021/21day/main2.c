#include <stdio.h>
#include <stdlib.h>

#define NEW(x, y)	((x + y) % 10 ? (x + y) % 10 : 10)

long long unsigned int p1_wins = 0;
long long unsigned int p2_wins = 0;

void first_roll_p1(short, short, short, short);
void first_roll_p2(short, short, short, short);
void secnd_roll_p1(short, short, short, short);
void secnd_roll_p2(short, short, short, short);
void third_roll_p1(short, short, short, short);
void third_roll_p2(short, short, short, short);

void roll_three_p1(short, short, short, short);
void roll_three_p2(short, short, short, short);

int final(int, int);

main(int argc, char** argv) {
	final(4, 8);
	printf("%llu, %llu\n", p1_wins, p2_wins);
	
	return 0;
}

// si fotta la memoizzazione, lo faccio brute force (spero che c faccia un minimo di TCO almeno)
void first_roll_p1(short p1, short score1, short p2, short score2) {
	int x = NEW(p1, 1);
	secnd_roll_p1(x, score1 + 1, p2, score2);
	x = NEW(p1, 2);
	secnd_roll_p1(x, score1 + 2, p2, score2);
	x = NEW(p1, 3);
	secnd_roll_p1(x, score1 + 3, p2, score2);	
}

void first_roll_p2(short p1, short score1, short p2, short score2) {
	int x = NEW(p2, 1);
	secnd_roll_p2(p1, score1, x, score2 + 1);
	x = NEW(p2, 2);
	secnd_roll_p2(p1, score1, x, score2 + 2);
	x = NEW(p2, 3);
	secnd_roll_p2(p1, score1, x, score2 + 3);
}

void secnd_roll_p1(short p1, short score1, short p2, short score2) {
	int x = NEW(p1, 1);
	third_roll_p1(x, score1 + 1, p2, score2);
	x = NEW(p1, 2);
	third_roll_p1(x, score1 + 2, p2, score2);
	x = NEW(p1, 3);
	third_roll_p1(x, score1 + 3, p2, score2);
}

void secnd_roll_p2(short p1, short score1, short p2, short score2) {
	int x = NEW(p2, 1);
	third_roll_p2(p1, score1, x, score2 + 1);
	x = NEW(p2, 2);
	third_roll_p2(p1, score1, x, score2 + 2);
	x = NEW(p2, 3);
	third_roll_p2(p1, score1, x, score2 + 3);
}

void third_roll_p1(short p1, short score1, short p2, short score2) {
	int x = NEW(p1, 1);
        	if (score1 + 1 >= 21)
		p1_wins++;
	else first_roll_p2(x, score1 + 1, p2, score2);
	x = NEW(p1, 2);
	if (score1 + 2 >= 21)
		p1_wins++;
            else first_roll_p2(x, score1 + 2, p2, score2);
	x = NEW(p1, 3);
	if (score1 + 3 >= 21)
		p1_wins++;
	else first_roll_p2(x, score1 + 3, p2, score2);
}

void third_roll_p2(short p1, short score1, short p2, short score2) {
	int x = NEW(p2, 1);
        	if (score2 + 1 >= 21)
		p2_wins++;
	else first_roll_p1(p1, score1, x, score2 + 1);
	x = NEW(p2, 2);
	if (score2 + 2 >= 21)
		p2_wins++;
            else first_roll_p1(p1, score1, x, score2 + 2);
	x = NEW(p2, 3);
	if (score2 + 3 >= 21)
		p2_wins++;
	else first_roll_p1(p1, score1, x, score2 + 3);
}


void roll_three_p1(short p1, short score1, short p2, short score2) {
	int i;
	if (score1 + 3 >= 21) p1_wins++;
	else roll_three_p2(NEW(p1, 3), score1 + 3, p2, score2);

	if (score1 + 4 >= 21) p1_wins += 3;
	else for (i = 0; i < 3; i++) roll_three_p2(NEW(p1, 4), score1 + 4, p2, score2);

	if (score1 + 5 >= 21) p1_wins += 6;
	else for (i = 0; i < 6; i++) roll_three_p2(NEW(p1, 5), score1 + 5, p2, score2);

	if (score1 + 6 >= 21) p1_wins += 7;
	else for (i = 0; i < 7; i++) roll_three_p2(NEW(p1, 6), score1 + 6, p2, score2);

	if (score1 + 7 >= 21) p1_wins += 6;
	else for (i = 0; i < 6; i++) roll_three_p2(NEW(p1, 7), score1 + 7, p2, score2);

	if (score1 + 8 >= 21) p1_wins += 3;
	else for (i = 0; i < 3; i++) roll_three_p2(NEW(p1, 8), score1 + 8, p2, score2);

	if (score1 + 9 >= 21) p1_wins += 1;
	else roll_three_p2(NEW(p1, 9), score1 + 9, p2, score2);

}



void roll_three_p2(short p1, short score1, short p2, short score2) {
	int i;
	if (score2 + 3 >= 21) p2_wins++;
	else roll_three_p1(p1, score1, NEW(p2, 3), score2 + 3);

	if (score2 + 4 >= 21) p2_wins += 3;
	else for (i = 0; i < 3; i++) roll_three_p1(p1, score1, NEW(p2, 4), score2 + 4);

	if (score2 + 5 >= 21) p2_wins += 6;
	else for (i = 0; i < 6; i++) roll_three_p1(p1, score1, NEW(p2, 5), score2 + 5);

	if (score2 + 6 >= 21) p2_wins += 7;
	else for (i = 0; i < 7; i++) roll_three_p1(p1, score1, NEW(p2, 6), score2 + 6);

	if (score2 + 7 >= 21) p2_wins += 6;
	else for (i = 0; i < 6; i++) roll_three_p1(p1, score1, NEW(p2, 7), score2 + 7);

	if (score2 + 8 >= 21) p2_wins += 3;
	else for (i = 0; i < 3; i++) roll_three_p1(p1, score1, NEW(p2, 8), score2 + 8);

	if (score2 + 9 >= 21) p2_wins += 1;
	else roll_three_p1(p1, score1, NEW(p2, 9), score2 + 9);

}


int final(int p1, int p2) {
	/* first_roll_p1(p1, 0, p2, 0); */
	roll_three_p1(p1, 0, p2, 0);
}
