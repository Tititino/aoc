class Cuboid {
	private final Point p1, p2;

	public Cuboid(Point p1, Point p2) {
		// probably should check some things
		this.p1 = new Point(p1);
		this.p2 = new Point(p2);
	}

	public boolean intersects(Cuboid c) {
		return true;
	}

	public Cuboid[] intersect(Cuboid c) {
		return null;
	}

	public Cuboid[] subtract(Cuboid c) {
		return null;
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof Cuboid) {
			Cuboid c = (Cuboid) o;
			return (c.p1 == this.p1) && (c.p2 == this.p2);
		}
		return false;
	}

	@Override
	public String toString() {
		return "(" + p1.get_x() + ".." + p2.get_x() + ", " + p1.get_y() + ".." + p2.get_y() + ", " + p1.get_z() + ".." + p2.get_z() + ") : Cuboid";
	}
}
