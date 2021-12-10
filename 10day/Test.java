import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.Scanner;
import java.util.Stack;

public class Test {

	public static void main(String args[]) {
		File file = new File(args[0]);
		
		try {
			System.out.println(final1(file));
			System.out.println(final2(file));
		} catch (FileNotFoundException e) {
			System.out.println("dumbass");
			return;
		}
	}

	public static int parse_word1(String str) {
		char ch = 'a';
		Stack<Character> s = new Stack<Character>();
            	for (char c: str.toCharArray())			
                        switch (c) {
				case '(': case '[': case '{': case '<':
					s.push(c);
					break;
				case ')':
					ch = s.pop();
					if (ch != '(') return 3;
					break;
				case ']':
					ch = s.pop();
					if (ch != '[') return 57;
					break;
				case '}':
					ch = s.pop();
					if (ch != '{') return 1197;
					break;
				case '>':
					ch = s.pop();
					if (ch != '<') return 25137;
					break;
		            	default:
					System.out.println("dumbass");
					return 0;
		}
		return 0;
	}

	public static long parse_word2(String str) {
		long acc = 0;
            	char ch = 'a';
		Stack<Character> s = new Stack<Character>();
            	for (char c: str.toCharArray())			
			switch (c) {
				case '(': case '[': case '{': case '<':
					s.push(c);
					break;
				case ')':
					ch = s.pop();
					if (ch != '(') return 0;
					break;
				case ']':
					ch = s.pop();
					if (ch != '[') return 0;
					break;
				case '}':
					ch = s.pop();
					if (ch != '{') return 0;
					break;
				case '>':
					ch = s.pop();
					if (ch != '<') return 0;
					break;
		            	default:
					System.out.println("dumbass");
					return 0;
			}
                        
			while (!s.empty()) {
				acc *= 5;
				switch (s.pop()) {
					case '(':
						acc += 1;
						break;
					case '[':
						acc += 2;
						break;
					case '{':
						acc += 3;
						break;
					case '<':
						acc += 4;
						break;
			}
		}
		return acc;
	}

	public static int final1(File file) throws FileNotFoundException {
		int acc = 0;
            	Scanner s = new Scanner(file);
		while (s.hasNextLine())
			try {
				acc += parse_word1(s.nextLine());
			} catch (EmptyStackException e) {
				continue;
			}
		return acc; 
	}


	public static long final2(File file) throws FileNotFoundException {
		long n = 0;
		ArrayList<Long> l = new ArrayList<Long>(90);
		Scanner s = new Scanner(file);
		while (s.hasNextLine())
			try {
				n = parse_word2(s.nextLine());
				if (n != 0)
					l.add(n);		// i could insert it in order but i kinda don't want to
			} catch (EmptyStackException e) {
				continue;
			}
		Collections.sort(l);
		if (l.size() != 0)
			return l.get(l.size() / 2);
		return 0;
	}
}
