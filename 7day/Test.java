public class Test {

	public static void main(String args[]) {
		File file = new File(args[0]);
                int[] arr = populate(file);
                System.out.println(final1(arr));
                // System.out.println(moda(arr));
                // System.out.println(mediana(arr); gotta try some statistical shit to see if i can not iterate over the whole list
        }

	public static int[] populate(File file) {
		Scanner s;
                try {
			s = new Scanner(file);
                } catch (FileNotFoundException e) {
                        System.out.printf("File not found");
                        return new int[0];
                }
	           
                String str;
                if (s.hasNextLine()) str = s.nextLine(); 
                else return new int[0];

                // maybe gonna try to do this w/ a stream and a map
                String[] split = str.split(",");
                int[] res = new int[split.length];
                for (int i = 0; i < split.length; i++)
		res[i] = Integer.valueOf(split[i]);           

                return res;
        }
            
        public static int final1(int[] arr) {
		return (new Crab(arr)).find_shortest_dist();
        }
}
