/* sum ints between a and b

  import sumints._

  scala> sumints(1,3)
  res0: Int = 6

  scala> sumints(1,4)
  res1: Int = 10

 */
object sumints {
  def sumints(a:Int,b:Int):Int = {
    if(a>b) 0 else a + sumints(a+1,b)
  }
}
