package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    print(countChange(300,List(5,10,20,50,100,200,500)) == 1022)
    print(countChange(301,List(2)) == 0)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
  {
    def isBalanced(left: Int, right: Int, str: List[Char]): Boolean =
      {
    	if (str.isEmpty) left == right
    	else if (left < right) false
    	else {
          val first = str.head
          if (first == '(') isBalanced(left + 1, right, str.tail)
          else if (first == ')') isBalanced(left, right + 1, str.tail)
          else isBalanced(left, right, str.tail) 
    	}
      }
    isBalanced(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  {
//    var counter = 0
//    def changes(m: Int, c: List[Int]): Unit =
//    {
//      if (!c.isEmpty){
//        if (m == 0){
//          counter += 1
//          return
//        } else {
//          val coin = c.head
//          if (m >= coin) changes(m - coin, c)
//          changes(m, c.tail)
//        }
//      }
//    }
//    changes(money, coins)
//    counter
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
