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
      if (c == 0) {
        1
      } else if (c == r) {
        1
      } else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isbalance(chars: List[Char], parth: List[Boolean]): Boolean = {
        if (chars.isEmpty) {
          parth.isEmpty
        } else if (chars.head == '(') {
          isbalance(chars.tail, false :: parth)
        } else if (chars.head == ')') {
          parth match {
            case h :: t => isbalance(chars.tail, t)
            case Nil => false
          }
        } else {
          isbalance(chars.tail, parth)
        }
      }
      isbalance(chars, List())
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else {
        coins match {
          case Nil => 0
          case head :: t => if (money < head) countChange(money, t) else (countChange(money, t) + countChange(money-head, coins))
        }
      }
    }
  }
