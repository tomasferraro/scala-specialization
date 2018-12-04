package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0 | c == 0 | r == c)
        return 1
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def _balance(char: List[Char], accum: Int): Int = {
        if (accum < 0) return accum

        char match {
          case '(' :: tail => _balance(tail, accum + 1)
          case ')' :: tail => _balance(tail, accum - 1)
          case _ :: tail => _balance(tail, accum)
          case _ => accum
        }
      }

      _balance(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0)
        1
      else if(money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else
        0
    }
  }
