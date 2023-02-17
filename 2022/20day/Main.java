import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Objects;
import java.util.LinkedList;
import java.lang.Math;

public class Main {

	private static int MAGIC_NUMBER = 811589153;
	
	public static void main(String[] args) {
		File input = new File(args[0]);
		Scanner scanner1 = null;
		Scanner scanner2 = null;
		try {
			scanner1 = new Scanner(input);
			scanner2 = new Scanner(input);

		} catch (FileNotFoundException e) {
			return;
		}

		System.out.println("part 1: " + part1(scanner1));
		System.out.println("part 2: " + part2(scanner2));
		
		scanner1.close();
		scanner2.close();
	}	

	private static long part1(Scanner s) {
		Objects.requireNonNull(s);
		LinkedList<Node> queue = parse1(s);
		mix(queue);	
		return solve(queue.getFirst());
	}

	private static long part2(Scanner s) {
		Objects.requireNonNull(s);
		LinkedList<Node> queue = parse2(s);
		for (int i = 0; i < 10; i++)
			mix(queue);
		return solve(queue.getFirst());
	}

	private static LinkedList<Node> parse1(Scanner s) {
		Objects.requireNonNull(s);

		LinkedList<Node> queue = new LinkedList<Node>();

		Node first; 
		Node prec;
		first = prec = new Node(s.nextLine());

		queue.addLast(first);

		while (s.hasNext()) {
			Node actual = new Node(s.nextLine());
			actual.setPrec(prec);
			queue.addLast(actual);
			prec = actual;
		}

		first.setPrec(queue.getLast());

		return queue;
	}

	private static LinkedList<Node> parse2(Scanner s) {
		Objects.requireNonNull(s);

		LinkedList<Node> queue = new LinkedList<Node>();

		Node first; 
		Node prec;
		first = prec = new Node(Long.valueOf(s.nextLine()) * MAGIC_NUMBER);

		queue.addLast(first);

		while (s.hasNext()) {
			Node actual = new Node(Long.valueOf(s.nextLine()) * MAGIC_NUMBER);
			actual.setPrec(prec);
			queue.addLast(actual);
			prec = actual;
		}

		first.setPrec(queue.getLast());

		return queue;
	}

	 private static void mix(LinkedList<Node> queue) {
	 	for (Node actual : queue) {
	 		for (int i = 0; i < (Math.abs(actual.getValue()) % (queue.size() - 1)); i++)
	 			if (actual.getValue() < 0)
	 				actual.swapPrec();
	 			else
	 				actual.swapSucc();
	 	}
	 }
	
	private static long solve(Node start) {
		Node actual = start;
		long tot = 0;
		while (actual.getValue() != 0)
			actual = actual.getSucc();

		for (int i = 0; i <= 3000; i++) {
			if (i == 1000 || i == 2000 || i == 3000)
				tot += actual.getValue();
			actual = actual.getSucc();
		}

		return tot;
	}

}
