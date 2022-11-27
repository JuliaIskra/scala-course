package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var balance: Int = 0
    var negative: Boolean = false
    chars.foreach(char => {
      if (char == '(') balance = balance + 1
      if (char == ')') balance = balance - 1
      if (balance < 0) {
        negative = true
      }
    })
    !negative && balance == 0
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def balance(from: Int, until: Int): Int = {
      var balance: Int = 0
      var i = from
      while (i < until && balance >= 0) {
        val char = chars(i)
        if (char == '(') balance = balance + 1
        if (char == ')') balance = balance - 1
        i = i + 1
      }
      balance
    }

    def reduce(from: Int, until: Int): Int = {
      if (until - from < threshold) {
        balance(from, until)
      } else {
        val mid = from + (until - from) / 2
        val (b1, b2) = parallel(
          reduce(from, mid),
          reduce(mid, until))
        if (b1 < 0) b1
        else b1 + b2
      }
    }

    reduce(0, chars.length) == 0

  // For those who want more:
  // Prove that your reduction operator is associative!

