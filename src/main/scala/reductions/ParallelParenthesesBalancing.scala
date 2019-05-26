package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance_num(chars: Array[Char],current_parens: Int): Boolean = {
      if(current_parens<0) false
      else if (chars.isEmpty && current_parens==0) true
      else if (chars.isEmpty && current_parens!=0) false
      else if(chars.head == '(')
        balance_num(chars.tail,current_parens+1)
      else if(chars.head == ')')
        balance_num(chars.tail,current_parens-1)
      else
        balance_num(chars.tail,current_parens)
    }
    balance_num(chars,0)

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {


    def traverse(myindex: Int, till: Int, close: Int, start: Int): (Int,Int) = {
      if (myindex == till)
        (close, start)
      else chars(myindex) match {
        case '(' => traverse(myindex + 1, till, close, start + 1)
        case ')' =>
          if (start > 0) traverse(myindex + 1, till, close, start - 1)
          else traverse(myindex + 1, till, close + 1, start)
        case _ => traverse(myindex + 1, till, close, start)
      }
    }

    def reduce(start: Int, end: Int):(Int,Int) = {
      if(end - start <= threshold)
        traverse(start, end, 0, 0)
      else {
        val mid = start + (end - start) / 2
        val ((left_closed, left_open), (right_closed, right_open)) = parallel(
          reduce(start, mid),
          reduce(mid, end))
        val closed = left_closed + (if ((right_closed - left_open) <= 0) 0 else right_closed - left_open)
        val open = if ((left_open - right_closed + right_open) < 0) 0 else left_open - right_closed + right_open
        (closed, open)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
