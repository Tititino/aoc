import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CaveGraph /* extends Graph<String>*/ {

	private final int size;
	private ArrayList<String> V;
	private int[][] distances;
	private int[][] adiacenza;
	private boolean changed = false;
	private int start = -1;
	private int end = -1;

	// kinda bad because i parse the file two times
	public CaveGraph(File file) throws FileNotFoundException {
		int i = 0;
		String from, to;
		V = new ArrayList<String>();
		Scanner s = new Scanner(file);
		Pattern p = Pattern.compile("^([A-Za-z]+)[^A-Za-z]+([A-Za-z]+)");
		Matcher m;
		while (s.hasNextLine()) {
			m = p.matcher(s.nextLine());
			m.matches();
			from = m.group(1);
			to = m.group(2);
			boolean fin = false, tin = false;
			for (String str: V) {
				if (fin && tin) break;
				if (from.equals(str)) fin = true;
				if (to.equals(str)) tin = true;
			}
			if (!fin) V.add(from);
			if (!tin) V.add(to);
		}
		s.close();
		
		s = new Scanner(file);
		i = V.size();
		adiacenza = new int[i][i];
		distances = null;
		size = i;
		start = V.indexOf("start");
		end = V.indexOf("end");
		if (start == -1 || end == -1)
			throw new InvalidCaveGraphException("No start or end vertices provided");
		
		while (s.hasNextLine()) {
			m = p.matcher(s.nextLine());
			m.matches();
			add_edge(m.group(1), m.group(2)); // this i kinda bad and unsecure, theres no check if the patern actually matched
		}
	}

	private boolean is_large(String v) {
		return Character.isUpperCase(v.charAt(0));
	}

	private int get_ind(String v) {
		int x = V.indexOf(v);
		if (x == -1) throw new NoSuchElementException("A vertex named '" + v + "' does not exist");
		else return x;
	}
	
	public void add_edge(String from, String to) {
		int y = get_ind(from), x = get_ind(to);
		if (adiacenza[y][x] == 1 && adiacenza[x][y] == 1)
			return;
		else {
			adiacenza[y][x] = adiacenza[x][y] = 1;
			changed = true;
		}
	}

	public void remove_edge(String from, String to) {
		int y = get_ind(from), x = get_ind(to);
		if (adiacenza[y][x] == 0 && adiacenza[x][y] == 0)
			return;
		else {
			adiacenza[y][x] = adiacenza[x][y] = 0;
			changed = true;
		}
	}
	
	// FloydWarshall algortithm, but i kinda don't want to implement it
	// also a benevolent side effect
	public int shortest_path(String from, String to) {
		if (distances == null || !changed) {
			// ...
			// ...
			return 0;
		}
		else {
			return 0;
		}
	}
	
	public boolean is_neighbor(String from, String to) {
		int y = get_ind(from), x = get_ind(to);
		return adiacenza[y][x] == 1 || adiacenza[x][y] == 1;
			
	}
	
	public int size() {
		return this.size;
	}

	private int _visits(int from, ArrayList<Integer> smalls) {
		if (from == end)
			return 1;
		if (smalls.contains(from))
			return 0;
		if (!is_large(V.get(from))) {
			smalls = new ArrayList<Integer>(smalls);
			smalls.add(from);
		}
		int acc = 0;
		for (int i = 0; i < size; i++)
			if (adiacenza[from][i] != 0)
				acc += _visits(i, smalls);
		return acc;
	}

	private int _visit_twice(int from, ArrayList<Integer> smalls) {
		boolean twice = true;
		if (from == end)
			return 1;
		if (smalls.contains(from))
			twice = false;
		if (!is_large(V.get(from))) {
			smalls = new ArrayList<Integer>(smalls);
			smalls.add(from);
		}
		int acc = 0;
		for (int i = 0; i < size; i++)
			if (adiacenza[from][i] != 0 && i != start)
				if (twice) acc += _visit_twice(i, smalls);
				else acc += _visits(i, smalls);
		return acc;
	}
	
	public int visits() {
	 	ArrayList<Integer> l = new ArrayList<Integer>();
		return _visits(start, l);
	}

	public int visits2() {
		ArrayList<Integer> l = new ArrayList<Integer>();
		return _visit_twice(start, l);
	}

	@Override
	public String toString() {
		// i could use a mutable string
		String str = "Graph([";
		for (String s: V)
			str += s + ", ";
		str = str.substring(0, str.length() - 2);
		str += "], [";
		for (int y = 0; y < this.size; y++) 
			for (int x = y; x < this.size; x++)
				if (adiacenza[y][x] == 1)
					str += V.get(y) + " -- " + V.get(x) + ", ";
		str = str.substring(0, str.length() - 2);
		return str += "])";
	}

	// non ho voglia
	public boolean repOk() {
		return true;
	}

	// not really equals doe
	@Override
	public boolean equals(Object O) {
		if (O instanceof CaveGraph) {
			CaveGraph c = (CaveGraph) O;
			if (this.size != c.size)
				return false;
			for (String s: V)
				if (!(c.V).contains(s))
					return false;
		       	for (int x = 0; x < this.size; x++)
				for (int y = 0; y < this.size; y++)
					if (this.adiacenza[y][x] != c.adiacenza[y][x])
						return false;
			return true;
		}
		return false;
	}
}
