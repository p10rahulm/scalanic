object ListOperations {

  // Get last element
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Exception("last of empty list does not exist")
    case List(x) => x
    case y :: ys => last[T](ys)
  }

  // Get all but last element
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  // concatenate two lists
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case List(x) => x :: ys
    case z :: zs => z :: concat(zs, ys)
  }

  // reverse a list
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case List(x) => List(x)
    case y :: ys => reverse(ys) ::: List(y)
    // complexity n^2
  }

  // remove element at position n
  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => throw new Exception("Nothing to remove. List empty")
    case List(x) => {
      if (n == 0) List()
      else throw new Exception("Index does not exist")
    }
    case y :: ys => {
      if (n < 0)
        throw new Exception("negative index")
      else if (n == 0)
        ys
      else
        y :: removeAt[T](n - 1, ys)
    }
  }

  def take[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => throw new Exception("Nothing to take. List empty")
    case List(x) => {
      if (n == 0) List()
      else throw new Exception("So many elements do not exist")
    }
    case y :: ys => {
      if (n < 0)
        throw new Exception("negative index")
      else if (n == 0)
        List()
      else
        y :: take[T](n - 1, ys)
    }
  }

  def drop[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => throw new Exception("Nothing to drop. List empty")
    case List(x) => {
      if (n == 0) List()
      else throw new Exception("So many elements do not exist")
    }
    case y :: ys => {
      if (n < 0)
        throw new Exception("negative index")
      else if (n == 0)
        ys
      else
        drop[T](n - 1, ys)
    }
  }
  def removeAt2[T](n: Int, xs: List[T]): List[T] = take[T](n,xs) ::: drop[T](n+1,xs)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => Nil
    case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }
  /*
    scala> import ListOperations._
    import ListOperations._

    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    res0: List[Any] = List(1, 1, 2, 3, 5, 8)

   */
  import math.Ordering

  def mergesort[T](xs: List[T])(implicit ord:Ordering[T]): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (left, right) = xs splitAt n
      merge(mergesort(left), mergesort(right))
    }
  }

  /*
  scala> println(mergesort(List(14, 2, 4, 1, 3)))
  List(1, 2, 3, 4, 14)

  scala> println(mergesort(List("pineapple","carrot","banana","ladies finger")))
  List(banana, carrot, ladies finger, pineapple)

  */
  //Scalelist scales each element of a list
  def scaleList(xs:List[Double],factor:Double):List[Double] = {
    if(xs.isEmpty)
      List()
    else if(xs.tail.isEmpty)
      xs.head * factor :: List()
    else
      xs.head*factor::scaleList(xs.tail,factor)
  }
  def map[T,U](f: T => U)(xs: List[T]):List[U] = xs match{
    case Nil => List()
    case y::ys => f(y) :: map(f)(ys)
  }

  def scaleList2(xs:List[Double],factor:Double):List[Double] = {
    map[Double,Double](x => x*factor)(xs)
  }
  def squareList(xs:List[Double],factor:Double):List[Double] = {
    map[Double,Double](x => x*x)(xs)
  }
  /*
    scala> scaleList2(List(1,2,3),2)
    res1: List[Double] = List(2.0, 4.0, 6.0)

    scala> squareList(List(1,2,3),3)
    res0: List[Double] = List(1.0, 4.0, 9.0)

   */
  def filter[T](f: T => Boolean)(xs: List[T]):List[T] = xs match {
    case Nil => List()
    case y::ys => {
      if(f(y))
        y :: filter(f)(ys)
      else
        filter(f)(ys)
    }
  }
  def posElems2(xs:List[Int]):List[Int] = xs match {
    case Nil => xs
    case y :: ys => if(y>0) y::posElems(ys) else posElems(ys)
  }
  def posElems(xs:List[Int]):List[Int] = {
    filter[Int](x => (x >= 0))(xs)
  }
  /*
  scala> posElems(List(-1,-2,0,1,20,-5))
  res1: List[Int] = List(0, 1, 20)

  scala> posElems2(List(-1,-2,0,1,20,-5))
  res2: List[Int] = List(0, 1, 20)

  Other List functions available on standard library are
  filterNot => items that are not filtered

  partition => filter :: filterNot

  takewhile => longest prefix of xs satisfying a predicate

  dropwhile => all other than longest prefix from takewhile

  span => (xs takewhile p, xs dropwhile p) splits xs into two parts of a tuple

  scala> List(-1,-2,0,1,20,-5) dropWhile (x=>x<0)
  res7: List[Int] = List(0, 1, 20, -5)

  scala> List(-1,-2,0,1,20,-5) takeWhile (x=>x<0)
  res8: List[Int] = List(-1, -2)

  scala> List(-1,-2,0,1,20,-5) span (x=>x<0)
  res9: (List[Int], List[Int]) = (List(-1, -2),List(0, 1, 20, -5))

   */

  /*
  pack(List("a", "a", "a", "b", "c", "c", "a"))
  should give
  List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
   */
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val first_pack = xs.takeWhile(input_T => input_T == x)
      val rest = xs.dropWhile(input_T => input_T == x)
      first_pack :: pack(rest)

    }
  }
  /*
  import ListOperations._

  scala> pack(List("a", "a", "a", "b", "c", "c", "a"))
  res0: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))

   */
  /*
  encode(List("a", "a", "a", "b", "c", "c", "a"))
  should give
  List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
   */
  def length[T](xs:List[T]):Int = {
    if(xs.isEmpty)
      0
    else
      1 + length(xs.tail)
  }
  def encode[T](given_seq:List[T]):List[(T,Int)] = {
    val packed = pack(given_seq)

    def give_output[T](given_seq:List[List[T]]):List[(T,Int)] = {
      if(given_seq.isEmpty)
        List()
      else{
        val inlist = given_seq.head
        val rest = given_seq.tail
        (inlist.head,length(inlist)) :: give_output(rest)
      }
    }

    give_output(packed)

  }
  /*

  scala> import ListOperations._
  import ListOperations._

  scala> encode(List("a", "a", "a", "b", "c", "c", "a"))
  res0: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

   */
}