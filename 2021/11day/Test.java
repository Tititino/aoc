import java.util.Arrays;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Test {

	public static void main(String[] args) {
		File file = new File(args[0]);
		try {
			System.out.println(final1(file, Integer.valueOf(args[1])));
			System.out.println(final2(file));
		} catch (FileNotFoundException e) {
			System.out.println("File not found");
			return;
		}	
	}

	public static void print_mat(int mat[][]) {
		for (int[] arr: mat)
			System.out.println(Arrays.toString(arr));
	}	

	public static int[][] populate(File file) throws FileNotFoundException {
		Scanner s = new Scanner(file);
		String str;
		int mat[][], x = 0, y = 0;
		if (s.hasNextLine()) {
			str = s.nextLine();
			mat = new int[str.length()][str.length()];
			for (char c: str.toCharArray())
				mat[0][x++] = c - '0';
		}
		else {
			return null;
		}
		
		while (s.hasNextLine()) {
			y++; x = 0;
			for (char c: s.nextLine().toCharArray())
				mat[y][x++] = c - '0';
			
		}

		return mat;
	}

	public static void flash(int mat[][], int flash[][], int x, int y) {
		mat[y][x]++;
		if (mat[y][x] > 9 && flash[y][x] == 0) {
			flash[y][x]++;
			if (x != 0)
				flash(mat, flash, x - 1, y);
			if (x != mat.length - 1)
				flash(mat, flash, x + 1, y);
			if (y != 0)
				flash(mat, flash, x, y - 1);
			if (y != mat.length - 1)
				flash(mat, flash, x, y + 1);
			if (y != 0 && x != 0)
				flash(mat, flash, x - 1, y - 1);
			if (y != 0 && x != mat.length - 1)
				flash(mat, flash, x + 1, y - 1);
			if (y != mat.length - 1 && x != 0)
				flash(mat, flash, x - 1, y + 1);
			if (y != mat.length - 1 && x != mat.length - 1)
				flash(mat, flash, x + 1, y + 1);
	        	}
	}

	public static int update(int mat[][]) {
		int x, y, acc = 0;
		for (y = 0; y < mat.length; y++)
			for (x = 0; x < mat.length; x++)
				mat[y][x]++;

		int[][] flash_m = new int[mat.length][mat.length];
		for (y = 0; y < mat.length; y++)
			for (x = 0; x < mat.length; x++)
				if (mat[y][x] > 9)
					flash(mat, flash_m, x, y);
		
	 	for (y = 0; y < mat.length; y++)
			for (x = 0; x < mat.length; x++)
				if (mat[y][x] > 9) {
					acc++;
					mat[y][x] = 0;
				}
		return acc;
	}

	public static int final1(File file, int cycles) throws FileNotFoundException {
		int mat[][] = populate(file);
		int acc = 0;
		while (cycles--  != 0) {
			acc += update(mat);
		}
		print_mat(mat);
		return acc;	
	}

	public static int final2(File file) throws FileNotFoundException {
		int mat[][] = populate(file);
		for (int c = 1; c < Integer.MAX_VALUE; c++) {
			if (update(mat) == mat.length * mat.length)
				return c;
		}
		return -1;
	}

}
