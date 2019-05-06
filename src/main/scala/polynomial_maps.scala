object polynomial_maps {

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap) //star here means it's a repeat parameter, we can pass an arbitrary number of concrete arguments
    val terms = terms0 withDefaultValue 0.0

    //how would we add two polynomials together? we'd need to merge coefficients that have the same exponent.
    //here's one way:
    //def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

    //def adjust(term: (Int, Double)): (Int, Double) = {
    //	val (exp, coeff) = term
    //	exp -> (coeff + terms(exp))
    //}

    //here's a more efficient way with fold left:
    def +(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  /*
    *
    * scala> import polynomial_maps._
      import polynomial_maps._
    *
    * scala> val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
      p1: polynomial_maps.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1

      scala> val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
      p2: polynomial_maps.Poly = 7.0x^3 + 3.0x^0

      scala> p1+p2
      res0: polynomial_maps.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
    *
   */


}
