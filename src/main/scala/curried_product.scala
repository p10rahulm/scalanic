/*  we try to build a product function that is curried. ie. from a to b we product the function.
    we will attempt to build such functions.
    productint(1,3)
    productsquares(1,3)
    productcubes(1,3)

    scala> fact(1,4)
    res1: Int = 24


 */


object curried_product {

  def productfn(f: Int => Int): (Int, Int) => Int = {
    def returnfunction(a: Int, b: Int): Int = {
      if (a > b) 1 else f(a) * returnfunction(a + 1, b)
    }
    returnfunction
  }

  def productint = productfn(x => x)
  def productsquares = productfn(x => x*x)
  def productcubes = productfn(x => x*x*x)
  def fact = productfn(x=>x)
  def productfact= productfn(factorial.factorial)
  def productfact2= productfn(x => factorial.factorial(x))

  def productcurried(f : Int=>Int)(a:Int,b:Int): Int ={
    if(a>b) 1 else f(a) * productcurried(f)(a+1,b)
  }


}
