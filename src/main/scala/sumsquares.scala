/*  sum the squares between a and b

    scala> import sumsquares._
    import sumsquares._

    scala> square(3)
    res1: Int = 9

    scala> sumsquares(1,3)
    res2: Int = 14

    scala> sumsquares(1,4)
    res3: Int = 30

 */
object sumsquares {
  def square(a:Int):Int = a * a
  def sumsquares(a:Int, b:Int):Int = {
    if(a>b) 0 else square(a) + sumsquares(a+1,b)
  }
}
