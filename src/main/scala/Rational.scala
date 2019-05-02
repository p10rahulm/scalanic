/*
    We are creating a new class for Rationals. Scala supports such objects

    scala> import Rational._
    import Rational._

    scala> val a= new Rational(1,2)
    a: Rational = Rational@4d84945d

    scala> a.numer
    res0: Int = 1

 */
class Rational(numerator:Int,denominator:Int) {
  def numer:Int = numerator
  def denom: Int = denominator

}
