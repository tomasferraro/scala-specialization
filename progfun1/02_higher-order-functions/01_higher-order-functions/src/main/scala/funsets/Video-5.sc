object rationals {

  class Rational(x: Int, y: Int) {
    require(y != 0, "Denominator must be non-zero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = if (<(that)) that else this

    def + (other: Rational): Rational = new Rational(numer * other.denom + other.numer * denom, denom * other.denom)
    def unary_- : Rational = new Rational(-numer, denom)
    def - (other: Rational): Rational = this + -other

    override def toString: String = s"$numer/$denom"
  }


  new Rational(1,2).+(new Rational(2,3))
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x + y
  x - y - z
  x < y
  x max y
}