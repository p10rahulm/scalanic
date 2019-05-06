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

  /*
  *
  * def sum(xs:List[Int]) and def product(xs:List[Int]) produce the sum and product respectively of all the elements in the list.
  * Now we try to implement these
  *
   */
  def sum(xs:List[Int]): Int = xs match {
    case Nil => 0
    case head::tail => head + sum(tail)
  }
  def product(xs:List[Int]): Int = xs match {
    case Nil => 1
    case head::tail => head * product(tail)
  }

  def sum2(xs: List[Int])     = (0 :: xs) reduceLeft ((x,y) => x + y)
  def product2(xs: List[Int]) = (1 :: xs) reduceLeft ((x,y) => x * y)

  def reduceRight[T](xs:List[T],f:(T, T) => T):T = xs match {
    case List(x) => x
    case y::ys => f(y,reduceRight[T](ys,f))
  }

  //foldRight unlike reduceRight takes into account an accumulator, which is z below

  def foldRight[T,U](xs:List[T],z: U)(op: (T,U) => U): U = xs match {
    case Nil => z
    case y :: ys => op(y,foldRight(ys,z)(op))
  }
  /*
    *
    * scala> import ListOperations._
      import ListOperations._

      scala> reduceRight(List(1,3,5),(x:Int,y:Int)=>x*y)
      res0: Int = 15
    *
    * scala> foldRight(List(1,3,5),0)((x:Int,y:Int)=>x*y)
      res1: Int = 0

      scala> foldRight(List(1,3,5),1)((x:Int,y:Int)=>x*y)
      res2: Int = 15


  */

  /*
   *
   *  FoldLeft (fold from left)
      The function reduceLeft is defined in terms of a more general function, foldLeft. It's like reduceLeft, but it takes an accumulator, or zero-element z, which is returned when foldLeft is called on an empty list.

      (List(x1, ..., xn) foldLeft z)(op) = (...(z op x1) op ...) op xn
      So,

      def	sum(xs: List[Int]) = (xs foldLeft 0) (_ + _)
      def product(xs: List[Int]) = (xs foldLeft 1) (_ * _)

      abstract class List[T] { ...
        def reduceLeft(op: (T, T) => T): T = this match {
          case Nil => throw new Error("Nil.reduceLeft")
          case x :: xs => (xs foldLeft x)(op)
        }
        def foldLeft[U](z: U)(op: (U,T) => U): U = this match {
          case Nil => z
          case x :: xs => (xs foldLeft op(z, x))(op)
        }
      }

   */

  def reduceLeft[T](xs:List[T],op: (T, T) => T):T = xs match {
    case Nil => throw new Error("Nil.reduceLeft")
    case y :: ys => foldLeft(ys,y)(op)
  }
  def foldLeft[T,U](xs:List[T],z: U)(op: (U,T) => U): U = xs match {
    case Nil => z
    case y :: ys => foldLeft(ys , op(z, y))(op)
  }
  /*
   *
   *  scala> import ListOperations._
      import ListOperations._

   *  scala> reduceLeft( List(1,2,3),(x:Int,y:Int) => x*y)
      res7: Int = 6

      scala> reduceLeft( List(1.0,2.0,3.0),(x:Double,y:Double) => x*y)
      res10: Double = 6.
   *
   *  scala> foldLeft(List(1,3,5),1)((x:Int,y:Int)=>x*y)
      res3: Int = 15


   */

  def concatenate[T](xs: List[T], ys: List[T]):List[T] = xs match {
    case Nil => ys
    case x :: Nil => x :: ys
    case z :: zs => z :: foldRight(zs,ys)((x,y)=> x :: y)
  }
  /*
   *  scala> import ListOperations._
   *
   *  scala> concatenate(List(1,2,3),List(4,5,6))
      res1: List[Int] = List(1, 2, 3, 4, 5, 6)

      scala> concatenate(List(1,2,3),List(4,5,'a'))
      res2: List[Int] = List(1, 2, 3, 4, 5, 97)

      scala> concatenate(List(1,2,7),List(4,5,'a'))
      res3: List[Int] = List(1, 2, 7, 4, 5, 97)
   *
   *  Note what happens for different types of lists
   *
   *  scala> concatenate(List('c',2,7),List('b','b','a'))
      res4: List[AnyVal] = List(99, 2, 7, b, b, a)

      scala> concatenate(List('c','s','p'),List('b','b','a'))
      res5: List[Char] = List(c, s, p, b, b, a)

   */
  def lengthFun[T](xs: List[T]): Int = foldRight(xs,0)((x,y)=> 1+y)
  /*
    *
    *   scala> import ListOperations._
        import ListOperations._

        scala> lengthFun(List(1,2,3,4.5))
        res1: Int = 4

        scala> lengthFun(List(1,2,3,4,5))
        res2: Int = 5

    *
   */

  def mapFun[T, U](xs: List[T], f: T => U): List[U] = foldRight(xs,List[U]())((x,y) => ( f(x) :: y ) )
  /*
    * scala> import ListOperations._
      import ListOperations._
    *
    * scala> mapFun[Int,Int](List(1,2,3,4,5),x => x + 2)
      res1: List[Int] = List(3, 4, 5, 6, 7)

    * scala> mapFun[Int,String](List(1,2,3,4,5),x => x + "2")
      res3: List[String] = List(12, 22, 32, 42, 52)

      scala> mapFun[Int,String](List(1,2,3,4,5),x => x + "some String")
      res4: List[String] = List(1some String, 2some String, 3some String, 4some String, 5some String)

      scala> mapFun[Int,String](List(1,2,3,4,5),x => x + " some String")
      res5: List[String] = List(1 some String, 2 some String, 3 some String, 4 some String, 5 some String)

    *
   */


}