
import org.scalameter._
import task_parallel._

/** A First Class Task */
sealed trait MyTask[+A] {
  def join: A

}

object MyTask {
  //parallel using task - want to compute cA and cB in parallel
  def parallel[A,B](cA: => A, cB: => B): (A,B) = {
    val tB = task(cB)
    val tA: A = cA
    (tA, tB.join)
  }
  //WRONG - does not compute tasks A and B in parallel
  def parallelWrong[A, B](cA: => A, cB: => B): (A,B) = {
    val tB: B = (task { cB }).join //joined too early
    val tA: A =  cA
    (tA, tB)
  }



}
