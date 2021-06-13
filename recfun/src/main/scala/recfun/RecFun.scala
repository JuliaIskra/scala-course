package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def balanceIter(chars: List[Char], parCount: Int): Int =
      if (chars.isEmpty) then return parCount

      if (parCount < 0) then return parCount

      if (chars.head == '(')
      then return balanceIter(chars.tail, parCount + 1)

      if (chars.head == ')')
      then return balanceIter(chars.tail, parCount - 1)

      return balanceIter(chars.tail, parCount)

    balanceIter(chars, 0) == 0

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 ||  coins.isEmpty) then return 0
    if (money == 0) then return 1
    countChange(money, coins.tail) + countChange(money - coins.head, coins)
