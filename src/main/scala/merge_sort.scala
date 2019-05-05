object merge_sort {
  def msort[T](xs: List[T])(less: (T, T) => Boolean): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (left, right) = xs splitAt n
      merge(msort(left)(less), msort(right)(less))
    }
  }

  /*
  import merge_sort._

  scala> println(msort(List(14, 2, 4, 1, 3))((x: Int, y: Int) => x < y))
  List(1, 2, 3, 4, 14)

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

}
