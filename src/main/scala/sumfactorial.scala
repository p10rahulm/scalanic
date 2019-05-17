/*
  we get the sum of factorials between a and b
  scala> import sumfactorial._
  import sumfactorial._

  scala> sumfact(1,3)
  res0: Int = 9

  scala> sumfact(1,4)
  res1: Int = 33


 */
import factorial._
object sumfactorial {
  def sumfact(a:Int,b:Int):Int = {
    if(a>b) 0 else factorial.factorial(a) + sumfact(a+1,b)
  }

}
