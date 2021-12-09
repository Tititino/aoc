import java.io.File;
import java.io.FileNotFoundException;
import java.lang.IndexOutOfBoundsException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Scanner;

public class Heightmap {

	// OVERVIEW:
	// This class rapresents a map of heights as a n x m matrix
	// where sizey = n and sizex = m.
	// Each entry of this matrix is a number between 0 and 9.
	// The map is immutable.

	// Attributes
	private ArrayList<int[]> map;
	private final int sizex;
	private final int sizey;
	private LinkedList<Coord> low_points;

	// Constructors
        /**
	  * Cnstructs a Heightmap from a file
	  * 	@param file contains the values to construct the Heightmap, each value must be between 0 and 9
	  *	@throws FileNotFoundExcpetion if the file is missing
	  */            
	public Heightmap(File file) throws FileNotFoundException {
		Scanner s = new Scanner(file);
		if (!s.hasNextLine()) {
			sizex = 0;
			sizey = 0;
			map = null;
			low_points = null;
			assert repOk();
			return;
		}
                        
		int x = 0, y = 1;

		String str = s.nextLine();
		sizex = str.length();
		map = new ArrayList<int[]>();
		map.add(new int[sizex]);
		for (char num: str.toCharArray())
			if (Character.isDigit(num))
				map.get(0)[x++] = num - '0';
			else
				throw new InvalidCharacterException("The character '" + num + "' does not rapresent a digit");
		x = 0;
		while (s.hasNextLine()) {
			map.add(new int[sizex]);
			str = s.nextLine();
			for (char num: str.toCharArray()) // kinda cringe ngl
				if (Character.isDigit(num))
					map.get(y)[x++] = num - '0';
				else
					throw new InvalidCharacterException("The character '" + num + "' does not rapresent a digit");
			x = 0;
			y++;
		}
		sizey = y;
		low_points = null;
		assert repOk();
	}

	// Methods
	/**
	  * Gets the value at $\{x, y\}
	  * 	@param x the $x$ coordinates of the point
	  * 	@param y the $y$ coordinates of the point
	  *	@return the value of the point at $\{x, y\}$
	  */
	public int get(int x, int y) throws IndexOutOfBoundsException {
		if (x < sizex && y < sizey) return map.get(y)[x];
		throw new IndexOutOfBoundsException(); 		// i don't think i really need this
	}

	/**
	  * Checks the neighbors of a point
	  * 	@param x the $x$ coordinates of the point
	  * 	@param y the $y$ coordinates of the point 
	  *	@param n the value of the point at $\{x, y\}$
          *	@return true if the point is a low point (see <code> get_low_poins </code>), false otherwise
	  */
	private boolean check_dirs(int x, int y, int n) {
		boolean res = true;
		if (y != 0) res &= n < get(x, y - 1);
		if (y != sizey - 1) res &= n < get(x, y + 1);
		if (x != 0) res &= n < get(x - 1, y);
		if (x != sizex - 1) res &= n < get(x + 1, y);
		return res;
	}

	/**
	  * Returns the list of of coordinates of the low points in <code> this </code>
	  * low points are defined as a point that is surrounded (only on the cardinal directions)
	  * by values larger than itself
	  *	@return a list containing all the low points coordinates in <code> this </code>
	  */  
	public LinkedList<Coord> get_low_points() {
		if (low_points != null) return low_points;
                        
            	low_points = new LinkedList<Coord>();
		int x = 0, y = 0;
		for (y = 0; y < sizey; y++) {
			for (x = 0; x < sizex; x++) {
				int n = map.get(y)[x];
             			if (check_dirs(x, y, n))
					low_points.add(new Coord(x, y));
			}
		}
		return low_points;
	}

	/**
	  * This function consist in the actual recursive part of <code> get_basin_size </code>
	  * 	@param x the $x$ coordinates of the point
	  * 	@param y the $y$ coordinates of the point
	  * 	@param mat is a matrix used to keep track of the visited points
	  *         @return the size of the basin as defined in the descripion of <code> get_basin_size </code>
	  */            
	private int _get_basin_size(int x, int y, int mat[][]) {
		if (get(x, y) == 9) return 0;
		int n = 1;
		mat[y][x] = 1;
		if (y != sizey - 1 && mat[y + 1][x] == 0)
			n += _get_basin_size(x ,y + 1, mat);
		if (y != 0 && mat[y - 1][x] == 0)
			n += _get_basin_size(x, y - 1, mat);
		if (x != sizex - 1 && mat[y][x + 1] == 0)
			n += _get_basin_size(x + 1, y, mat);
		if (x != 0 && mat[y][x - 1] == 0)
			n += _get_basin_size(x - 1, y, mat);
		return n;
	}
            
	/** 
	  * returns the size of the basin that contains the point $\{x, y\}$
	  * A basin is defined as a region of space in the Heightmap delimited by 9s
	  * the size of a basin is defined as the number of points contained by it
	  * 	@param x the $x$ coordinates of the point, an invalid coordinate results in an IndexOutOfBoundsExcpetion
	  * 	@param y the $y$ coordinates of the point, an invalid coordinate results in an IndexOutOfBoundsExcpetion
	  * 	@return the size of the basin as defined in the descripion
	  */            
	public int get_basin_size(int x, int y) {
		if (get(x, y) == 9) return 0;
			return _get_basin_size(x, y, new int[sizey][sizex]);
	}
                        
	@Override
	public String toString() {
		String str = "";
		for (int[] arr: map)
			str += Arrays.toString(arr) + "\n";
		return str;
	}


	/*
	 * the rep invariant is: sizex \geq 0 \wedge sizey \geq 0 \wedge (map = \text{\texttt{null}} \rightarrow sizex = 0 \vee sizey = 0) \wedge 
	 * map:Mat_{n\times m} (sizey = n \wedge sizex = m) \wedge 
	 * \forall y : \text{\texttt{int}} \text{ }\forall x : \text{\texttt{int}} 
	 * (y < sizey \wedge x < sizex \rightarrow map_{[y,x]} \geq 0 \wedge map_{[y,x]} \leq 9)
	 */
	public boolean repOk() {
		if (sizex < 0 || sizey < 0) return false;
		if (map == null && sizex != 0 && sizex != 0) return false;
		if (map.get(0).length != sizex) return false;
		if (map.size() != sizey) return false;
		for (int[] arr: map)
			for (int i: arr)
				if (i < 0 || i > 9)
					return false;
		return true;
	}
}
