/* Check whether list of chars has balanced parantheses
   run like import balance_parantheses._

   balance(List(')','(','(',')'))
   balance(List('(','(','(',')'))
   balance(List('(',')','(',')'))
   balance(List('(','(','a',')'))
   balance(List('(','(','a',')',')'))

*/

object balance_parantheses {
  def balance(chars: List[Char]): Boolean = {
    def bracket_change(head_char: Char): Int = {
      if (head_char == '(')
        +1
      else if (head_char == ')')
        -1
      else
        0
    }

    def isbalanced(chars: List[Char], num_open: Int): Boolean = {
      if (num_open < 0 || num_open > 0 && chars.isEmpty)
        false
      else if (num_open == 0 && chars.isEmpty)
        true
      else {
        isbalanced(chars.tail, num_open + bracket_change(chars.head))
      }
    }

    isbalanced(chars, 0)

  }

}
