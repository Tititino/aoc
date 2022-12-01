import java.io.File;
import java.io.FileNotFoundException;

public class Test {
	public static void main(String[] args) {
		File file = new File(args[0]);
		CaveGraph g;
		try {
			g = new CaveGraph(file);
		} catch (FileNotFoundException e) {
			System.out.println("File not found");
			return;
		}
		System.out.println(g);
		System.out.println(g.visits());
		System.out.println(g.visits2());	
	}
}
