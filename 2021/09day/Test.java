import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.Scanner;

public class Test {
	public static void main(String args[]) {
		File file = new File(args[0]);
                Heightmap map;
                try {
                      	map = new Heightmap(file);
		} catch (FileNotFoundException e) {
			System.out.println("File not found");
          		return;
                }
                                    
		System.out.println(final1(map));
		System.out.println(final2(map));
	}

	
	public static int final1(Heightmap m) {
		int acc = 0;
		for (Coord c: m.get_low_points())
			acc += m.get(c.get_x(), c.get_y()) + 1;
		return acc;
	}

	public static int final2(Heightmap m) {
		int max1 = 1, max2 = 1, max3 = 1;
                        
		for (Coord c: m.get_low_points()) {
			int n = m.get_basin_size(c.get_x(), c.get_y());
			if (n > max1) {
				max3 = max2;
				max2 = max1;
				max1 = n;
			}
			else if (n > max2) {
				max3 = max2;
				max2 = n;
			}
			else if (n > max3)
				max3 = n;
		}
		return max1 * max2 * max3;
	}
}
