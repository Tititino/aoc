// should probably be a record
class Point {
	private final int x, y, z;

	public Point() {
		x = 0;
		y = 0;
		z = 0;
	}

	public Point(int x, int y, int z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public Point(Point p) {
		this.x = p.x;
		this.y = p.y;
		this.z = p.z;
	}

	public int get_x() {
		return this.x;
	}

	public int get_y() {
		return this.y;
	}

	public int get_z() {
		return this.z;
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof Point) {
			Point p = (Point) o;
			return (this.x == p.x) && (this.y == p.y) && (this.z == p.z);
		}
		return false;
	}

	@Override
	public String toString() {
		return "(" + this.x + ", " + this.y + ", " + this.z + ") : Point";
	}
}
