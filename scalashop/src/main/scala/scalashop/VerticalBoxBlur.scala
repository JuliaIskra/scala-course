package scalashop

import org.scalameter.*

object VerticalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")


/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface:

  /** Blurs the columns of the source image `src` into the destination image
   * `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   * Within each column, `blur` traverses the pixels by going from top to
   * bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var x = from
    while (x < end) {
      var y = 0
      while (y < src.height) {
        val blurred = boxBlurKernel(src, x, y, radius)
        dst.update(x, y, blurred)
        y += 1
      }
      x += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   * Parallelization is done by stripping the source image `src` into
   * `numTasks` separate strips, where each strip is composed of some number of
   * columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val size = src.width
    val actualNumTasks = Math.min(size, numTasks)

    val splitPoints = {
      val splits = (0 until size)
        .by(Math.ceil(size.toDouble / actualNumTasks).toInt)
      if (splits.contains(size)) {
        splits
      } else {
        splits :+ size
      }
    }

    splitPoints.zip(splitPoints.tail)
      .map((start, end) => task {
        blur(src, dst, start, end, radius)
      })
      .foreach(_.join)
  }

