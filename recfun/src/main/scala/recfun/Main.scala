package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    //println(balance("())(".toList))
    //println(countChange(4,List(1,2,3)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c < 0 || r < 0 ) 0
      else if ( r == 0 ||   c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c,r-1)
    }



  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def isBalanced(chars: List[Char], count: Int): Int =  {
        if (count < 0) -1 // This is to indicate mismatch of order correctly
        else if (chars.isEmpty) count
        else if (chars.head == '(') isBalanced(chars.tail, count + 1)
        else if (chars.head == ')') isBalanced(chars.tail, count - 1)
        else isBalanced(chars.tail, count)
      }

      if (chars.isEmpty)
        true
      else
        isBalanced(chars, 0) == 0 // all additions and subtractions should reduce it to zero
    }



  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      // (4, (1,2,3))
      // (4,(2,3),ans) + ( 4 -1, (1,2,3), '1 + ')
      // ((4, (3), ans) + (2, (2,3), 2 + )) + ((3,(2,3),'1+') + (3-1, (1,2,3), '1 + 1'))
      // So take head coin start an iteration , start another iteration with rest of coins.

      def checkLeftMoney(leftmoney: Int, coins: List[Int], str: String): Int =  {
      // How many ways can you give change for 0 CHF(swiss money)?
        if (leftmoney == 0) 1
        else if (leftmoney < 0) 0
        else if (coins.isEmpty) 0
        else (checkLeftMoney(leftmoney - coins.head, coins, str + " + " + coins.head) + checkLeftMoney(leftmoney, coins.tail, str))
      }
      checkLeftMoney(money, coins, "")
    }
  }





