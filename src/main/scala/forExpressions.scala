object forExpressions {

  /*
    *
    * Find all sequences of integers (i,j) such that 1 <= j < i < N  and i+j is prime
    * Note that we have less than N and less than i and not less than equals.
    *
    * What is the imperative way of doing this? using for loops of course.
    * But what is the funcitonal programming way? We generate a data structure containing all possible
    * versions of (x,y) in a collection and then filter that list.
    * 1. Find a range
    * scala> (1 until 10).map(x => println(x))
      1
      2
      3
      4
      5
      6
      7
      8
      9
      res11: scala.collection.immutable.IndexedSeq[Unit] = Vector((), (), (), (), (), (), (), (), ())
    *
    * 2. Now we generate all the pairs that we need into a data structure
    *
    * scala> val n = 5
      n: Int = 5

      scala> (1 until n).map(i => (1 until i).map(j => (i,j)))
      res13: scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), Vector((4,1), (4,2), (4,3)))

    * 3. Now we have to filter such pairs based on the condition we want
    *
    *

   */
  def isPrime(n: Int): Boolean = {
    (2 to n - 1).forall(x => n % x != 0)
  }

  def forloop_i_to_n(n: Int)(f: (Int, Int) => Boolean) = {
    ((1 until n).flatMap(i => (1 until i).map(j => (i, j)))).filter(pair => f(pair._1, pair._2))
  }

  def primepairs(n: Int) = {
    forloop_i_to_n(n)((x: Int, y: Int) => isPrime(x + y))
  }

  def primepairs2(n: Int) = {
    for {
      i <- 1 until n
      j <- 1 until i
      if (isPrime(i + j))
    } yield (i, j)
  }

  def scalarProduct3(xs: List[Double], ys: List[Double]): Double = {
    (
      for {
        (x, y) <- xs.zip(ys)
      } yield x * y
      ).sum
  }

  /*
    * scala> import forExpressions._
      import forExpressions._

    * scala> forloop_i_to_n(4)((x:Int,y:Int) => isPrime(x+y))
      res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2))
    *
    * scala> import forExpressions._
      import forExpressions._

      scala> primepairs(4)
      res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2))

    * scala> primepairs2(4)
      res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2))

    *
    *
    * scala> scalarProduct3(List(1,2,3),List(4,5,6))
      res0: Double = 32.0

    *
   */



}
