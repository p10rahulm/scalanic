/*  we try to build a sum function that is curried. ie. from a to b we sum the function.
    we will attempt to build such functions.
    sumint(1,3)
    sumsquares(1,3)
    sumcubes(1,3)
    sumfact(1,3)

    or directly
    sumfn(x=>x)(1,2)
    sumfn(factorial.factorial)(1,2)
    (sumfn(factorial.factorial))(1,2)

    Use the currying function syntax
    sumcurried(factorial.factorial)(1,2)


 */


object curried_sum {

  def sumfn(f: Int => Int): (Int, Int) => Int = {
    def returnfunction(a: Int, b: Int): Int = {
      if (a > b) 0 else f(a) + returnfunction(a + 1, b)
    }
    returnfunction
  }

  def sumint = sumfn(x => x)
  def sumsquares = sumfn(x => x*x)
  def sumcubes = sumfn(x => x*x*x)
  def sumfact= sumfn(factorial.factorial)
  def sumfact2= sumfn(x => factorial.factorial(x))

  def sumcurried(f : Int=>Int)(a:Int,b:Int): Int ={
    if(a>b) 0 else f(a) + sumcurried(f)(a+1,b)
  }


}
