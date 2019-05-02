/*  recursive function that counts how many different ways you can make change for an amount,
    given a list of coin denominations. For example, there are 3 ways to give change for 4
    if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.

    run like import splitting_coins._

    countChange(2,List(1,2))
    countChange(3,List(1,2))
    countChange(0,List(1,2))
    countChange(4,List(1,2))

*/
object splitting_coins {

  def countChange(money: Int, coins: List[Int]): Int = {
    if(money<0 || coins.isEmpty)
      0
    else if(money==0) 1
    else {
      countChange(money,coins.tail) + countChange(money - coins.head,coins)
    }
  }


}
