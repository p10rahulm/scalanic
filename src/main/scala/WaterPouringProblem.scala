/*
  *
  * The task is to find a purely functional solution to the Water Pouring Problem. The problem is found in the course by Peter Norvig,
  * called Designs of Computer Programs and Udacity where Peter develops a very nice solution in Python:
  * https://www.youtube.com/watch?v=q6M_pco_5Vo
  *
  * Problem
    We have a faucet, a sink, and no. of unmarked glasses of different sizes. We want to get the given amount of water in a glass.
  * Solution
    States and Moves
    We have n no. of glasses.

    Glass: glass number (from 0 to n-1): Int
    State: amount of water in a glass: Vector[Int] of size n. (one entry per glass)
    Moves:
    Empty(glass)
    Fill(glass)
    Pour(from, to)
    Example:
    Lets say we have 2 glasses: glass 0 and glass 1 with capacity 4 and 9. If we fill glass 0, we go from state (0/0) to state (4/0).
    Now we pour from 0 to 1 which gives us state (0/4).

    The idea is to start at (0/0), then generate all possible states of length 1 (using 1 move), then generate all possible states of length 2
    and so on until we reach our target or we are out of solutions.
  *
 */
class Pouring (capacity : Vector[Int]) {

  //States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  //Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move{
    def change(state: State) = state updated ( glass, 0)
  }
  case class Fill(glass: Int) extends Move{
    def change(state: State) = state updated ( glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move{
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from)- amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length
  val moves = (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  //Paths
  class Path(history: List[Move], val endState : State) {
    //def endState :State = (history foldRight initialState) (_ change _)
    def extend(move :Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ")+ "-->" + endState
  }

  val initialPath = new Path(Nil,initialState)

  def from(paths :Set[Path], explored: Set[State]) :Stream[Set[Path]] =
    if(paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solution(target :Int) :Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

object Pouring {
  def main(args :Array[String]){
    val problem = new Pouring(Vector(4, 9, 19))
    println(problem.moves)
    println(problem.solution(16))
  }
}