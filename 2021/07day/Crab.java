import java.util.ArrayList;
import java.util.stream.IntStream;

public class Crab extends ArrayList<Integer> {
	private ArrayList<Integer> els;
	private final int max;
        private final int min;

	public Crab(int[] arr) {
            	if (arr.length == 0) {
                        els = null;
			max = 0;
                        min = 0;
                }
                else {
            	        int temp_max = arr[0], temp_min = arr[0];
			els = new ArrayList<Integer>(arr.length);
                        for (int i: arr) {
				if (i > temp_max) temp_max = i;
                        	            if (i < temp_min) temp_min = i;
                        	            els.add(i);
                        	}
                                max = temp_max;
                                min = temp_min;
                }
        }

	private int abs(int n) {
		return (n < 0) ? -n : n;
        }
            
        public int find_shortest_dist() {
            	int min_dist = Integer.MAX_VALUE;
                for (int i = min; i <= max; i++) {
                        int acc = 0;
			for (int c: els) {
				int n = abs(c - i);
                                        acc += (n * (n + 1)) / 2;
                        }
                        if (acc < min_dist) min_dist = acc;
                 }
                 return min_dist;
        }
            
}
