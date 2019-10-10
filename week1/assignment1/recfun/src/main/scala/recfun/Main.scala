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
      if (c == 0) 1
      else if (r == 0) 0
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = balanceHelper(chars, List[Char]())

    def balanceHelper(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) stack.isEmpty
      else if (chars.head == '(') balanceHelper(chars.tail, stack :+ chars.head)
      else if (chars.head == ')') {
        if (stack.isEmpty) false
        else balanceHelper(chars.tail, stack.init)
      } 
      else balanceHelper(chars.tail, stack)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 0
  }
