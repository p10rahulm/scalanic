object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      if(computeDelta(a, b, c)()<0){
        Set[Double]()
      } else if(computeDelta(a, b, c)() == 0) {
        Set[Double](-b()/2/a())
      } else {
        Set[Double](
          (-b() + Math.sqrt(delta())) / (2 * a()),
          (-b() - Math.sqrt(delta())) / (2 * a())
        )
      }
    }
  }
}
