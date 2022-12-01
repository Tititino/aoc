import java.util.Scanner;
import java.util.LinkedList;
import java.util.ArrayList;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.File;
import java.util.NoSuchElementException;
import java.util.Arrays;
import java.util.Iterator;

public class Test {
	
	// OVERVIEW: this sucks ass, this is actual dogshit, i won't even bother to clean this up
	
	public static void main(String[] args) {
        	if (args.length == 0) throw new RuntimeException("No file provided");
		int[] acc = new int[25];
		LinkedList<Integer> queue = new LinkedList<Integer>();
		Scanner oof;
                try {
                	oof = new Scanner(new File(args[0]));
                } catch (FileNotFoundException e) {
			System.out.println("File not found");
                        return;
                }
		ArrayList<Bingo_board> bingos = new ArrayList<Bingo_board>();
		Bingo_board board, last_board = null;

                populate(queue, oof);
                        
		while (!oof.hasNextInt()) oof.next();
                                                
		while (oof.hasNextLine()) {
			populate_board(acc, oof);
                       	board = new Bingo_board(acc, 5);
                        ingos.add(board);
               	}

               	Integer last_num = 0;
                while (!queue.isEmpty()) {
                	Integer num = queue.poll();
                        if (num == null) return;
			
			Iterator<Bingo_board> it = bingos.iterator();
                        while (it.hasNext()) {
                               	Bingo_board b = it.next();
                        	b.insert_entry(num);
                        	for (int i = 0; i < b.size(); i++)
                        	      	if (b.check_column(i) || b.check_row(i)) {
                                                last_board = b;
						last_num = num;
                                                it.remove();
                                                break;
                        	       	}
                        }
                }

        	System.out.println(last_board.sum_res() * last_num);
      	}
	
	public static void populate(LinkedList<Integer> queue, Scanner s) {
		String line = s.nextLine();
                for (String num: line.split(","))
                	if (num != null) 
                       		queue.add(Integer.valueOf(num));
      	}

	public static void populate_board(int[] arr, Scanner s) {
		if (arr.length != 25) return;
            	int i = 0;
               	for (int k = 0; k < 5; k++)
			for (; i < 25 && s.hasNextInt(); i++) {
                        	arr[i] = s.nextInt();
                                try {
                                	while (!s.hasNextInt()) s.next();
                                } catch (NoSuchElementException e) {
					break;
                                }
                     	}
      	}
}
