object nqueens {
/*
  *
  *   Problem
        The eight queens problem is to place eight queens on a chessboard so that no queen is threatened by another.

        In other words, there can’t be two queens in the same row, column, or diagonal. We now develop a solution for a chessboard of any size, not just 8.
        Eg.
              0      1     2     3
           -------------------------
        0  |     |  X  |     |     |
           -------------------------
        1  |     |     |     |  x  |
           -------------------------
        2  |  x  |     |     |     |
           -------------------------
        3  |     |     |  X  |     |
           -------------------------
        One way to solve the problem is to place a queen on each row. When we have placed k - 1 queens, one must place the kth queen in a column where it’s not “in check” with any other queen on the board. Eg. 0th queen doesn't matter. So if the first quen is placed as above in the (0,1) the 2nd goes in (1, 3), and so on.

      Algorithm
        We can solve this problem with a recursive algorithm:

        Suppose that we have already generated all the solutions consisting of placing k-1 queens on a board of size n.
        Each solution is represented by a list (of length k-1) containing the numbers of columns (between 0 and n-1).
        The column number of the queen in the k-1the row comes first in the list, followed by the column number of the queen in row k-2, etc.
        The solution set is thus represented as a set of lists, with one element for each solution.
        Now, to place the kth queen, we generate all possible extensions of each solution preceded by a new queen:
              0      1     2     3
           -------------------------
        0  |     |  X  |     |     |
           -------------------------
        1  |     |     |     |  x  |
           -------------------------
        2  |  x  |     |     |     |
           -------------------------
        3  |     |     |  X  |     |
           -------------------------
        So using he algorithm above, the solution using the first 3 queens would be list(0, 3, 1)because we placed:

        3rd queen in (2, 0)
        2nd in (1, 3)
        1st in (0,1) So placing the 4th queen, our solution will be list(1, 0, 3, 1)


      Implementation:
  *
 */
  def queens(n: Int):Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }
  /*
    *
    * If k == 0 return empty list. Now, in the case where k is greater than 0 we have to do some real work. In general, to place k queens, we have to solve the problem of placing k - 1 queens. We'll let queens range over the set of our partial solutions returned by placeQueens(k - 1).

      Next, we have to put our k queen into a certain column. We can simply try all the possible columns - col <- 0 until n. We can't place the queen in any column we please, we need to make sure it doesn't threaten any other queen. So, we'll put a filter in there, that says that the column for the queen is safe with respect to the previous queens. (isSafe(col, queens))

      If it is, then we can yield a new solution, which will be our partial solution col, augmented by the queen in the new column. So it would be col :: queens.

      Next we implement isSafe:
    *
   */
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    //find no of rows
    val row = queens.length
    //find (row,column) pairs
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    /*
      *
      * make sure no rook type move by col != c
      * make sure no bishop type move with
      * math.abs(col - c) != row - r
      *
    */
    queensWithRow forall {
      case (r, c) => col != c  && math.abs(col - c) != row - r
    }
  }

  /*
    *
    * scala> import nqueens._
      import nqueens._

      scala> queens(4)
      res0: Set[List[Int]] = Set(List(2, 0, 3, 1), List(1, 3, 0, 2))

      scala> queens(5)
      res1: Set[List[Int]] = Set(List(0, 3, 1, 4, 2), List(2, 0, 3, 1, 4), List(0, 2, 4, 1, 3), List(2, 4, 1, 3, 0), List(1, 3, 0, 2, 4), List(3, 0, 2, 4, 1), List(4, 2, 0, 3, 1), List(4, 1, 3, 0, 2), List(3, 1, 4, 2
      , 0), List(1, 4, 2, 0, 3))

      scala> queens(6)
      res2: Set[List[Int]] = Set(List(2, 5, 1, 4, 0, 3), List(3, 0, 4, 1, 5, 2), List(4, 2, 0, 5, 3, 1), List(1, 3, 5, 0, 2, 4))

      scala> queens(8)
      res4: Set[List[Int]] = Set(List(2, 0, 6, 4, 7, 1, 3, 5), List(5, 2, 0, 7, 3, 1, 6, 4), List(4, 1, 7, 0, 3, 6, 2, 5), List(4, 2, 7, 3, 6, 0, 5, 1), List(4, 2, 0, 5, 7, 1, 3, 6), List(2, 6, 1, 7, 4, 0, 3, 5), Lis
      t(3, 5, 7, 1, 6, 0, 2, 4), List(2, 5, 1, 6, 4, 0, 7, 3), List(4, 0, 7, 3, 1, 6, 2, 5), List(3, 5, 7, 2, 0, 6, 4, 1), List(6, 3, 1, 7, 5, 0, 2, 4), List(5, 2, 6, 1, 7, 4, 0, 3), List(3, 7, 0, 2, 5, 1, 6, 4), Lis
      t(2, 5, 3, 1, 7, 4, 6, 0), List(3, 1, 6, 2, 5, 7, 0, 4), List(7, 2, 0, 5, 1, 4, 6, 3), List(0, 6, 3, 5, 7, 1, 4, 2), List(1, 7, 5, 0, 2, 4, 6, 3), List(1, 5, 0, 6, 3, 7, 2, 4), List(3, 7, 0, 4, 6, 1, 5, 2), Lis
      t(3, 7, 4, 2, 0, 6, 1, 5), List(5, 2, 4, 7, 0, 3, 1, 6), List(7, 3, 0, 2, 5, 1, 6, 4), List(5, 2, 6, 3, 0, 7, 1, 4), List(2, 4, 1, 7, 5, 3, 6, 0), List(0, 5, 7, 2, 6, ...


    *
   */
  def show(queens:List[Int]) = {
    val lines =
      for(col <- queens.reverse) yield
        Vector.fill(queens.length)("* ").updated(col,"X ").mkString
    "\n\n" + (lines mkString("\n")) + "\n\n"

  }
  /*
    *
    * scala> queens(4).map(show)
      res3: scala.collection.immutable.Set[String] =
      Set("

      * X * *
      * * * X
      X * * *
      * * X *

      ", "

      * * X *
      X * * *
      * * * X
      * X * *

      ")


    *
    * scala> queens(5).map(show)
      res4: scala.collection.immutable.Set[String] =
      Set("

      * * * * X
      * X * * *
      * * * X *
      X * * * *
      * * X * *

      ", "

      * * * X *
      * X * * *
      * * * * X
      * * X * *
      X * * * *

      ", "

      * * X * *
      X * * * *
      * * * X *
      * X * * *
      * * * * X

      ", "

      * * * * X
      * * X * *
      X * * * *
      * * * X *
      * X * * *

      ", "

      * * * X *
      X * * * *
      * * X * *
      * * * * X
      * X * * *

      ", "

      X * * * *
      * * * X *
      * X * * *
      * * * * X
      * * X * *

      ", "

      * X * * *
      * * * X *
      X * * * *
      * * X * *
      * * * * X

      ", "

      X * * * *
      * * X * *
      * * * * X
      * X * * *
      * * * X *

      ", "

      * X * * *
      * * * * X
      * * X * *
      X * * * *
      * * * X *

      ", "

      * * X * *
      * * * * X
      * X * * *
      * * * X *
      X * * * *

      ")


    *
    * scala> (queens(4).map(show)).mkString
      res10: String =
      "

      * X * *
      * * * X
      X * * *
      * * X *



      * * X *
      X * * *
      * * * X
      * X * *

      "
    *
    * scala> (queens(8) take 3 map show).mkString
      res12: String =
      "

      * * * * X * * *
      * * * * * * X *
      * X * * * * * *
      * * * X * * * *
      * * * * * * * X
      X * * * * * * *
      * * X * * * * *
      * * * * * X * *



      * * * * * X * *
      * * * X * * * *
      * X * * * * * *
      * * * * * * * X
      * * * * X * * *
      * * * * * * X *
      X * * * * * * *
      * * X * * * * *



      * * * * * X * *
      * * X * * * * *
      * * * * * * X *
      * * * X * * * *
      X * * * * * * *
      * * * * * * * X
      * X * * * * * *
      * * * * X * * *

      "


    *
   */
}
