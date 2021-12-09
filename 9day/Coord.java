// i hate that Java just doesn't have tuples
public record Coord(int x, int y) {
	public Coord(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public int get_x() {
		return x;
	}

	public int get_y() {
		return y;
	}
            
	@Override
	public String toString() {
		return "<" + x + ", " + y + ">";
	}
}
