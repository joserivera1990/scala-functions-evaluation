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
  }

  /***
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || r == 0 || r == c) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)



  /**
   * Exercise 2
   */
    def balance(chars: List[Char]) : Boolean = {
      if (chars.isEmpty)
        return true

        @tailrec
        def checkRightSide(chars: List[Char],  parenthesisLeft: Int): Boolean =
          if (chars.isEmpty) {
            return true;
          } else if (chars.head.toString.equals("(")) {
            checkRightSide(chars.tail, parenthesisLeft + 1)
          } else if (chars.head.toString.equals(")")) {
            if (parenthesisLeft > 0)
              checkRightSide(chars.tail, parenthesisLeft - 1)
            else
              return false
          }
          else {
            checkRightSide(chars.tail, parenthesisLeft);
          }

      checkRightSide(chars, 0);
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
