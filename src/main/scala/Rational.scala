/*
    We are creating a new class for Rationals. Scala supports such objects

    scala> import Rational._
    import Rational._

    scala> val a = new Rational(1,2)
    a: Rational = 1/2

    scala> val b = new Rational(2,3)
    b: Rational = 2/3


    scala> a.add(b)
    res0: Rational = 7/6

    scala> a.add(b).toString
    res1: String = 7/6

    scala> a.add(b).makeString
    res2: String = 7/6

    scala> a.mult(b)
    res3: Rational = 2/6

    scala> a.divide(b)
    res4: Rational = 3/4

    scala> a.sub(b)
    res5: Rational = -1/6

    scala> val y = new Rational(5,7)
    y: Rational = 5/7

    scala> val z = new Rational(3,2)
    z: Rational = 3/2

    scala> z.max(y)
    res0: Rational = 3/2

    scala> z.less(y)
    res1: Boolean = false

    scala> y.add(z)
    res5: Rational = 31/14
    // equivalent notation of above is where you don't use the dot model

    scala> z add y
    res3: Rational = 31/14

    scala> y add z
    res4: Rational = 31/14

    scala> y + z
    res4: Rational = 31/14



 */
class Rational(numerator: Int, denominator: Int) {
  require(denominator>0,"Denominator must be positive")
  private val greatest_divisor = gcd.gcd(numerator,denominator)
  def numer: Int = numerator/greatest_divisor
  def denom: Int = denominator/greatest_divisor

  //  allow single input to class using denominator as 1
  def this(x:Int) = this(x,1)

  def add(otherRational: Rational): Rational = {
    new Rational(numer * otherRational.denom + denom * otherRational.numer, denom * otherRational.denom)
  }

  //  we can name an identifier using symbols also!
  def + (otherRational: Rational): Rational = {
    new Rational(numer * otherRational.denom + denom * otherRational.numer, denom * otherRational.denom)
  }

  def sub(otherRational: Rational): Rational = {
    new Rational(numer * otherRational.denom - denom * otherRational.numer, denom * otherRational.denom)
  }

  def mult(otherRational: Rational): Rational = {
    new Rational(numer * otherRational.numer, denom * otherRational.denom)
  }

  //  a.divide(b) is a/b
  def divide(otherRational: Rational): Rational = {
    new Rational(numer * otherRational.denom, denom * otherRational.numer)
  }

  def neg:Rational = new Rational(-numer,denom)

  // a.less(b) is true if a is less than b
  def less(otherRational:Rational):Boolean = {
    numer*otherRational.denom < denom*otherRational.numer
  }
  // a.max(b) returns max of a and b
  def max(otherRational:Rational):Rational = {
    if(this.less(otherRational)) otherRational else this
  }

  def makeString:String = {
    numer + "/" + denom
  }

  override def toString:String = {
    numer + "/" + denom
  }

}


