import java.lang.IndexOutOfBoundsException;

public class Bingo_board {
  	private int size;
        private int[] board;

	public Bingo_board(int[] board, int size) {
		this.size = size;
                this.board = new int[board.length];
               	for (int i = 0; i < size * size; i++)
                       	this.board[i] = board[i];
      	}

        public boolean insert_entry(int v) {
		int i;
             	for (i = 0; i < size * size; i++)
                       	if (v == board[i]) {
                        	board[i] = -1;
                                return true;
                        }

            	return false;
       	}

       	public boolean check_column(int n) {
		if (n > size) throw new IndexOutOfBoundsException("Column value too large");

            	for (int i = n; i < size * size; i += size)
			if (board[i] != -1) return false;
                        	return true;
  	}

   	public boolean check_row(int n) {
           	if (n > size) throw new IndexOutOfBoundsException("Row value too large");

             	for (int i = 0; i < size; i++)
                       if (board[(n * size) + i] != -1) return false;

              	return true;
 	}

        public int sum_res() {
		int acc = 0;
                        
            	for (int n: board)
                        if (n != -1) acc += n;

             	return acc;
        }

        public int size() {
		return size;
       	}

       	@Override
        public String toString() {
          	String s = "";
		for (int i = 0; i < size * size; i++) {
			if (i % size == 0)
                             	s += "\n";
                       	s += board[i] + " ";
                }
                return s;
       	}
}

