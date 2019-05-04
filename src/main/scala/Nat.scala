/*   These are implementations of natural numbers
 *   as successors of other numbers. These representations
 *   are called Peano Numbers. Note that all natural numbers
 *   are represented through other natural numbers.
 *
 *   Starting from Peano numbers, it is possible to define
 *   other number types like Integers (including negative numbers)
 *   and then even floating point numbers
 *
 *    import Nat._
 *
 *    scala> val a = Zero
      a: Zero.type = 0

      scala> val b = a.successor
      b: Succ = 1 + 0

      scala> val c = b.successor
      c: Nat = 1 + 1 + 0

      scala> val d = c.successor
      d: Nat = 1 + 1 + 1 + 0

      scala> val e = d.successor
      e: Nat = 1 + 1 + 1 + 1 + 0

 *    scala> a + b
      res0: Nat = 1 + 0

      scala> a + c
      res1: Nat = 1 + 1 + 0

      scala> a + d
      res2: Nat = 1 + 1 + 1 + 0

      scala> b + c
      res3: Nat = 1 + 1 + 1 + 0

      scala> b + d
      res4: Nat = 1 + 1 + 1 + 1 + 0

 *    scala> d-b
      res5: Nat = 1 + 1 + 0

      scala> e-b
      res6: Nat = 1 + 1 + 1 + 0

      scala> b-e
      java.lang.Error: no negative numbers allowed

 *
 *
 *
 *
 */

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("Zero has no predecessor")
  def successor = new Succ(this)

  def +(that: Nat):Nat = that
  def -(that: Nat):Nat = {
    if(that.isZero)
      this
    else throw new Error("no negative numbers allowed")
  }

  override def toString: String = "0"
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor:Nat
  def +(that:Nat):Nat
  def -(that:Nat):Nat

}


class Succ(num : Nat) extends Nat  {
  def isZero = false
  def predecessor: Nat = num
  def successor:Nat = new Succ(this)
  def +(that:Nat):Nat = {
    new Succ(num + that)
  }
  def -(that:Nat):Nat = {
    if(that.isZero)
      this
    else
    num - that.predecessor
  }
  override def toString: String = "1 + " + num.toString

}