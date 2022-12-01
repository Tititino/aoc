import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Test {
	public static void main(String[] args) {
		File file = new File(args[0]);
                        try {
			int[][] mat = populate(file);
                        	System.out.println(find_path(mat, 0, 0, mat[0].length, mat.length));
                        } catch (FileNotFoundException e) {
			System.out.println("dumbass");
                                    return;
                        }
	}

	public static int[][] populate(File f) throws FileNotFoundException {
		int n = 1, m = 0, y = 0, x;
		Scanner oof = new Scanner(f);
		if (oof.hasNextLine()) m = (oof.nextLine()).length();
		while (oof.hasNextLine()) {
			oof.nextLine();
			n++;
		}
		int[][] matrix = new int[n][m];
		oof.close();

		
		oof = new Scanner(f);
		while (oof.hasNextLine()) {
			x = 0;
			for (char c: (oof.nextLine()).toCharArray())
				matrix[y][x++] = c - '0';
			y++;
		}
		return matrix;
	}


	public static int find_path(int[][] mat, int xstart, int ystart, int xend, int yend) {
		int x, y, k, res = 0;;
		int[][] d = new int[mat.length][mat[0].length];
		
		for (y = 0; y < d.length; y++)
			for (x = 0; x < d[0].length; x++)
				if (x != xstart && y != ystart)
					d[y][x] = Integer.MAX_VALUE;
					
		for (k = 1; k < mat.length * mat[0].length; k++) {
			for (y = 0; y < d.length; y++) {
				for (x = 0; x < d[0].length; x++) {
					if (x > 0)
						if (d[y][x] != Integer.MAX_VALUE && d[y][x] + mat[y][x - 1] < d[y][x - 1])
							d[y][x - 1] = d[y][x] + mat[y][x - 1];
					if (x < mat[0].length - 1)
						if (d[y][x] != Integer.MAX_VALUE && d[y][x] + mat[y][x + 1] < d[y][x + 1])
							d[y][x + 1] = d[y][x] + mat[y][x + 1];
					if (y > 0)
						if (d[y][x] != Integer.MAX_VALUE && d[y][x] + mat[y - 1][x] < d[y - 1][x])
							d[y - 1][x] = d[y][x] + mat[y - 1][x];
					if (y < mat.length - 1)
						if (d[y][x] != Integer.MAX_VALUE && d[y][x] + mat[y + 1][x] < d[y + 1][x])
							d[y + 1][x] = d[y][x] + mat[y + 1][x];
				}
			}
		}
		return d[yend - 1][xend - 1];
	}


}
