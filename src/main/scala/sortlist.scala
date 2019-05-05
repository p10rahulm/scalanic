/*
  *   Here we create
  *   two methods (one main method and a helper method) for sorting a list
  *   Uses insertion sort. takes time n^2
 */
object sortlist {
  def isort(xs:List[Int]):List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y,isort(ys))
  }
  def insert(x:Int, xs:List[Int]):List[Int] = xs match {
    case List() => List(x)
    case y :: ys => {
      if(x < y)
        x :: xs
      else
        y :: insert(x,ys)
    }
   }

}
