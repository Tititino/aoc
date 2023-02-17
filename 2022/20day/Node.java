import java.util.Objects;

public class Node {
	private Node prec;
	private Node succ;

	private final long value;

	public Node(long value) {
		this.value = value;
	}	

	public Node(String value) {
		Objects.requireNonNull(value);
		this.value = Long.valueOf(value);
	}

	public long getValue() {
		return this.value;
	}

	public Node getSucc() {
		return this.succ;
	}

	public Node getPrec() {
		return this.prec;
	}

	public void setSucc(Node succ) {
		Objects.requireNonNull(succ);
		succ.prec = this;
		this.succ = succ;
	}

	public void setPrec(Node prec) {
		Objects.requireNonNull(prec);
		prec.succ = this;
		this.prec = prec;
	}

	public void swapPrec() {
		Node precPrecSucc = this.prec.prec.succ;
		Node thisSuccPrec = this.succ.prec;
		Node precPrec = this.prec.prec;
		Node precSucc = this.prec.succ;
		Node thisPrec = this.prec;
		Node thisSucc = this.succ;
		Node thisNode = this;

		this.prec.prec.succ = this;
		this.prec.succ      = thisSucc;
		this.prec.prec      = this;
		this.succ.prec      = thisPrec;
		this.succ           = thisPrec;
		this.prec           = precPrec;
	}

	public void swapSucc() {
		this.succ.swapPrec();
	}

	@Override
	public String toString() {
		return "" + this.value;
	}

	public String printNodes() {
		StringBuilder str = new StringBuilder();
		Node actual = this;
		while (!this.equals(actual.getSucc())) {
			str.append(actual.getValue() + ", ");
		}
		str.append(actual.getValue());
		return str.toString();
	}
}
