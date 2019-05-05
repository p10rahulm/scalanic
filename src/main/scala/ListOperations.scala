object ListOperations {

  // Get last element
  def last[T](xs:List[T]):T = xs match {
    case List() =>  throw new Exception("last of empty list does not exist")
    case List(x) => x
    case y::ys => last[T](ys)
  }
  // Get all but last element
  def init[T](xs: List[T]):List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y::ys => y :: init(ys)
  }
}
