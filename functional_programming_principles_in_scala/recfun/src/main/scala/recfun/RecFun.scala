package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(list: List[Char], n: Int): Boolean = (list, n) match{
        case (Nil, 0) => true
        case (_, n) if n < 0 => false
        case ('(' :: xs, n) => balanceHelper(xs, n + 1) 
        case (')' :: xs, n) => balanceHelper(xs, n - 1)
        case (x :: xs, n) => balanceHelper(xs, n)
        case (_, _) => false
    }
    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 0
}
