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
    def pascal(c: Int, r: Int): Int =
      if (c==0 | c==r) 1 else (pascal(c-1,r-1)+pascal(c,r-1))
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance_inner(chars: List[Char], sum : Int): Boolean =
        if (chars.isEmpty && sum == 0) true
        else if (chars.isEmpty && sum != 0) false
        else if (sum < 0) false
        else {
          if (chars.head == ')')  balance_inner(chars.tail, sum-1)
          else if (chars.head == '(') balance_inner(chars.tail, sum+1)
          else balance_inner(chars.tail, sum)
        }
      balance_inner(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count_inner(money: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0
        else if (money - coins.head == 0) 1
        else if (money - coins.head < 0) 0
        else count_inner(money - coins.head, coins) + count_inner(money, coins.tail)
      }

      count_inner(money, coins.sorted)
    }
  }
