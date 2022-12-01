import java.util.Arrays;

public class Board {
	private final int size;
	private int[][] matrix;

	// Constructors
	public Board(int n) {
		size = n;
		matrix = new int[size][size];
	}

	// Methods
	private int abs(int x) {
		return (x >= 0) ? x : -x;
	}
            
	public void addLine(int x1, int y1, int x2, int y2) {
		int stepx, stepy;
		if (x1 == x2 || y1 == y2) {
			stepy = (y1 == y2) ? 0 : 1;
			stepx = (x1 == x2) ? 0 : 1;
		}
		else {
			if (abs(x2 - x1) < abs(y2 - y1)) {
				stepx = 1;
				stepy = abs(y2 - y1) / abs(x2 - x1);
			}
			else {
				stepy = 1;
				stepx = abs(x2 - x1) / abs(y2 - y1);
			}
		}
		if (x1 > x2) stepx *= -1;
		if (y1 > y2) stepy *= -1;
		while ((x2 - x1) != 0 || (y2 - y1) != 0) {
			matrix[y1][x1]++;
			x1 += stepx;
			y1 += stepy;
		}
		matrix[y1][x1]++;
	}

	public int check(int n) {
		int acc = 0;
		for (int y = 0; y < size; y++)
			for (int x = 0; x < size; x++)
				if (matrix[y][x] >= n) acc++;
		return acc;                                               
	}

	public String toString() {
		String str = "";
		for (int y = 0; y < size; y++)
			str += Arrays.toString(matrix[y]) + "\n";
		return str;
	}
}
