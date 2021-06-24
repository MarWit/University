package numbers

class Rational(val numer: Int, val denom: Int) {
  require(denom >= 0, "denom should be >= 0")

  def +(other: Rational): Rational =
    new Rational(numer * other.denom + other.numer * denom, denom * other.denom)

  def -(other: Rational): Rational =
    new Rational(numer * other.denom - other.numer * denom, denom * other.denom)

  def *(other: Rational): Rational =
    new Rational(numer * other.numer, denom * other.denom)

  def /(other: Rational): Rational =
    new Rational(numer * other.denom, denom * other.numer)

  def toDouble: Double = numer.toDouble / denom.toDouble

  override def toString(): String = {
    val div = gcd(numer, denom);
    val new_denom = denom / div;
    val new_numer = (numer / div) % new_denom;
    val full = (numer / div) / new_denom;

    if(full == 0 && new_numer == 0) "0"
    else if(full == 0) s"$new_numer/$new_denom"
    else if(full > 0 && new_numer == 0) full.toString
    else s"$full $new_numer/$new_denom"
  }

  private def gcd(a: Int, b: Int): Int =
    if(b == 0) a
    else gcd(b, a % b)
}

object Rational {
  def ZERO: Rational = new Rational(0, 1)
  def ONE: Rational = new Rational(1, 1)
  def apply(integer: Int): Rational = new Rational(integer, 1)
}
