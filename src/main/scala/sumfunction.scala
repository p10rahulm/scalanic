/*
  we are going to sum from a to be some function = Σ f(x) from a to b
  scala> import sumfunction._
  import sumfunction._

  scala> import factorial._
  import factorial._

  scala> sumfunction(factorial,1,2)
  res3: Int = 3

  scala> sumfunction(factorial,1,3)
  res4: Int = 9


  scala> def id(x:Int) = x
  id: (x: Int)Int

  scala> def sumInts(a:Int,b:Int) = sumfunction(id,a,b)
  sumInts: (a: Int, b: Int)Int

  scala> sumInts(1,3)
  res6: Int = 6

  scala> sumInts(1,4)
  res7: Int = 10

  Anonymous functions:
  scala> def sumCubes(a:Int,b:Int) = sumfunction((x:Int)=>x*x*x,a,b)
  sumCubes: (a: Int, b: Int)Int

  scala> sumCubes(1,3)
  res8: Int = 36

  scala> sumCubes(1,4)
  res9: Int = 100

  Using tail recursive sumfunction

  scala> import sumfunction._
  import sumfunction._

  scala> sumf(x=>x*x,1,3)
  res1: Int = 14

  scala> sumf(x=>x*x,1,4)
  res2: Int = 30



 */

object sumfunction {
  def sumfunction(myfunc:Int=>Int,a:Int,b:Int):Int = {
    if(a>b) 0 else myfunc(a) + sumfunction(myfunc,a+1,b)
  }
  def sumInts(a:Int,b:Int) = sumfunction((x:Int)=>x,a,b)

  // Doing above tail recursively (no stack overflow)
  def sumf(f: Int => Int,a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }
}
