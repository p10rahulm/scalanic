object Collections {
  val xs = Array(1,2,3,44)
  xs.map(x=>x*2)

  val ys: String = "Hello world!"
  ys filter (_.isUpper)

  val nums = Vector(1, 2, 3, -88)
  val people = Vector("Bob","Peter","James")

  //  x +: xs Create a new vector with leading element x, followed by all elements of xs.
  //  xs :+ x Create a new vector with trailing element x, preceded by all elements of xs.

  val r: Range = 1 until 5   // 1, 2, 3, 4
  val s: Range = 1 to 5      // 1, 2, 3, 4, 5
  1 to 10 by 3               // 1, 4, 7, 10
  6 to 1 by -2               // 6, 4, 2


  /*
  p = (x => x > 0)
  xs exists p  // true if there is an element x of xs such that p(x) holds, false otherwise.
  xs forall p  // true if p(x) holds for all elements x of xs, false otherwise.
  xs zip ys    // A sequence of pairs drawn from corresponding elements of sequences xs and ys.
  xs.unzip     // Splits a sequence of pairs xs into two sequences consisting of the first, respectively second halves of all pairs
  xs.flatMap f // Applies collection-valued function f to all elements of xs and concatenates the results
  xs.sum       // The sum of all elements of this numeric collection.
  xs.product   // The product of all elements of this numeric collection
  xs.max       // The maximum of all elements of this collection (an Ordering must exist)
  xs.min       // The minimum of all elements of this collection
 */

  // List all combinations of x and y where x is taken from 1..N and y is taken from 1..M
  val N = 7
  val M = 9
  (1 to N) flatMap(x=>(1 to M) map(y=> (x,y)))

  /*
    *
    * scala> (1 to 5) flatMap(x=>(1 to 10) map(y=> (x,y)))
      res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8), (1,9), (1,10), (2,1), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (2,10), (3,
      1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7), (3,8), (3,9), (3,10), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7), (4,8), (4,9), (4,10), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6), (5,7), (5,8), (5,9), (5,10))
   */

  def scalarProduct(xs: Vector[Double],ys:Vector[Double]):Double = {
    ((xs.zip(ys)) map (xy=>xy._1 * xy._2)).sum
  }
  def scalarProduct2(xs: Vector[Double],ys:Vector[Double]):Double = {
    ((xs.zip(ys)) map {case (x,y) =>x*y}).sum
  }

  def isPrime(n:Int): Boolean = {
    (2 to n-1).forall(x => n%x !=0)
  }
  /*
    *
    * scala> import Collections._
      import Collections._

      scala> isPrime(2)
      res0: Boolean = true

      scala> isPrime(3)
      res1: Boolean = true

      scala> isPrime(4)
      res2: Boolean = false

      scala> isPrime(5)
      res3: Boolean = true

      scala> isPrime(6)
      res4: Boolean = false

      scala> isPrime(7)
      res5: Boolean = true

      scala> isPrime(8)
      res6: Boolean = false
    *
   */
}
