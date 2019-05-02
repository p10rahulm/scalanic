object sqrt {
  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(input: Double): Double = {

    def better_guess(input: Double, guess: Double): Double = {
      (guess + input / guess) / 2
    }

    def guess_is_good(input: Double, guess: Double): Boolean = {
      abs(input - guess * guess) < 0.001
    }

    def sqrtiter(input: Double, guess: Double): Double = {
      if (guess_is_good(input, guess))
        guess
      else
        sqrtiter(input, better_guess(input, guess))
    }

    if (input < 0)
      throw new Exception("you have a problem")
    else
      sqrtiter(input, 1)
  }
}
