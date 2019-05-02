/*  our curried sum function and curried product function differ in two things, the zero value
    and the operation that they do to the function outputs, either a sum or a product

    scala> import mapReduce._
    import mapReduce._

    scala> mapReduce(x=>x,(x,y)=>x*y,1)(1,4)
    res0: Int = 24

    scala> mapReduce(x=>x*x,(x,y)=>x+y,0)(1,4)
    res1: Int = 30

    scala> mapReduce(factorial.factorial,(x,y)=>x+y,0)(1,4)
    res2: Int = 33

    scala> def sumfact = mapReduce(factorial.factorial,(x,y)=>x+y,0)(_,_)
    sumfact: (Int, Int) => Int

    scala> sumfact(1,3)
    res3: Int = 9

    Properly currying now

    scala> def fact = mapReduce(x=>x,(x,y)=>x*y,1)(1,_)
    fact: Int => Int

    scala> fact(4)
    res5: Int = 24

    scala> fact(6)
    res6: Int = 720



 */
object mapReduce {
  def mapReduce(f: Int => Int, combine_operator: (Int, Int) => Int, zeroVal: Int)(a: Int, b: Int): Int = {
    if (a > b) zeroVal else combine_operator(f(a), mapReduce(f,combine_operator,zeroVal)(a + 1, b))
  }
}
