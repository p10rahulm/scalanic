object factorial {
  def factorial(n: Int): Int = {
    def fact_loop(acc: Int, n: Int): Int = {
      if (n == 0)
        acc
      else
        fact_loop(acc * n, n - 1)
    }

    fact_loop(1, n)
  }

}
