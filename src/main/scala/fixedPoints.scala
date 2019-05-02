
/*
    Fixed Points are those where the function crosses over with the y=x line

    scala> import fixedPoints._
    import fixedPoints._

    scala> fixedPoint
    fixedPoint   fixedPoints

    scala> fixedPoint(x=>1+x/2)(1)
    res0: Double = 1.998046875

    For sqrt, the solution is where the line crosses over with y = x/y
    def sqrt(x:Double)  = fixedPoint(y => x/y)(2)
    But above doesn't converge for x = 2. Why? because oscillation is too fast. Can we average the current and next value?

    def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(2)
    This works!

    scala> sqrt(2)
    res1: Double = 1.4142156862745097

    Same can be done by composing the (y => (y + x / y) / 2) as an average damped y=> x/y function.
    This is implemented in sqrt2

    scala> sqrt2(2)
    res3: Double = 1.4142156862745097

    scala> sqrt2(20)
    res4: Double = 4.4721402170657


 */

object fixedPoints {
  val tolerance = 0.001

  def isclose(x: Double, y: Double): Boolean = (absolute.absolute(x - y) / x) < tolerance

  def fixedPoint(f: Double => Double)(guess: Double): Double = {
    if (isclose(guess, f(guess)))
      guess
    else
      fixedPoint(f)(f(guess))
  }

  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(2)

  def averageDamp(f:Double=>Double)(x:Double) = (x+f(x))/2
  def sqrt2(x:Double):Double = fixedPoint(averageDamp(y=>x/y))(1)

}
