/*
 *   The ConsList is a fundamental data structure in many languages starting with Scheme. It is a constructed list type
 *   It is also known as the immutable linked list
 *
 *   It is constructed from two building blocks:
 *   Nil    the empty list
 *   Cons   a cell containing an element and the remainder of the list
 *   Note that lists containing lists, means the element itself is a list
 *
 *   The issue is that we don't want to define a new signature for each type, so we leave type as a parameter.
 *   The Type parameter would be passed in square brackets
 *   scala> import ConsList._
     import ConsList._

     scala> val a = new cons[Int](1,new nil[Int])
     a: ConsList.cons[Int] = ConsList$cons@719d868

     scala> val b = new cons[Int](2,a)
     b: ConsList.cons[Int] = ConsList$cons@67d383ad
 *
 *    scala> val x = singleton(1)
      x: ConsList.List[Int] = ConsList$cons@5c7c15db

      The types of singleton can be explicit as below or implicit as above (the compiler finds type)
      scala> val y = singleton[Boolean](true)
      y: ConsList.List[Boolean] = ConsList$cons@154533d3
 *
 *
 */

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  /*    The below are not required because we have already defined val in the arguments list
  *     val is a special case of a def or function definition
  *     def head:T = head
  *     def tail:List[T] = tail
  */
}

class nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("This is a nil element with no head")

  def tail: Nothing = throw new NoSuchElementException("This is a nil element with no tail")
}

// we can use a method like the below to create a list
def singleton[T](elem: T): List[T] = {
  new cons[T](elem, new nil[T])
}
/*
* Write a function nth that takes an integer n and a list and selects the n'th element of the list.
* Elements are numbered from 0.
* If index is outside the range from 0 up to the length of the list minus one, a IndexOutOfBoundsException should be thrown.
*
* scala> val a = new cons[Int](1,new nil[Int])
  a: ConsList.cons[Int] = ConsList$cons@11bd984d

  scala> val b = new cons[Int](2,a)
  b: ConsList.cons[Int] = ConsList$cons@1af29814

  scala> val c = new cons[Int](3,b)
  c: ConsList.cons[Int] = ConsList$cons@46f78f67

* scala> nth(0,c)
  res6: Int = 3

  scala> nth(1,c)
  res7: Int = 2

  scala> nth(2,c)
  res8: Int = 1
*
*
*/
def nth[T](n: Integer, mylist: List[T]): T = {
  def loop[T](current: Integer, mylist: List[T]): T = {
    if (mylist.isEmpty || n < 0) throw new IndexOutOfBoundsException("Please check the index")
    else if (current == 0) mylist.head
    else
      loop(current - 1, mylist.tail)
  }

  loop(n, mylist)

}

object List {
  /*
  List(1,2) = List.apply(1,2)
  */

  def apply[T](x:T,y:T): List[T] = new Cons(x, new Cons(y, new nil))

  /*
  List(1) = List.apply(1)
  */

  def apply[T](x:T): List[T] = new Cons(x, new nil)

  /*
  List() = List.apply()
  */

  def apply[T](): List[T] = new nil

}

