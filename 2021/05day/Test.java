import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Test {
	public static void main(String[] args) {
		if (args.length == 0) throw new RuntimeException("No file provided");
		Board matrix = new Board(Integer.valueOf(args[1])); 
		File file = new File(args[0]);
		populate(matrix, file); 

		if (Integer.valueOf(args[1]) < 20) System.out.println(matrix);
                        
		System.out.println(matrix.check(2));
            }

	public static final int UNICODE_CHARACTER_CLASS = 1;
	
	public static boolean filter1(int x1, int y1, int x2, int y2) {
		return !(x1 != x2 && y1 != y2);
	}

	public static boolean filter2(int x1, int y1, int x2, int y2) {
		return filter1(x1, y1, x2, y2) || ((x2 - x1) == (y2 - y1)) || ((x2 - x1) == -(y2 - y1));
	}

	public static void populate(Board b, File file) {
		int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
		Scanner oof;
		try {
			oof = new Scanner(file);
		} catch (FileNotFoundException e) {
			System.out.println("File not found");
			return;
		}
		Pattern p = Pattern.compile("^([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)");
		Matcher m;
		String line;
		while (oof.hasNextLine()) {
			m = p.matcher(oof.nextLine());
			m.matches();
			x1 = Integer.valueOf(m.group(1));
			y1 = Integer.valueOf(m.group(2));
			x2 = Integer.valueOf(m.group(3));
			y2 = Integer.valueOf(m.group(4));
			if (filter2(x1, y1, x2, y2))
				b.addLine(x1, y1, x2, y2);	
		}
	}
}
