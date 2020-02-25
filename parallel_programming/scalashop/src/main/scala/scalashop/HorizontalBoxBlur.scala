package scalashop

import org.scalameter._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
    for {
      y <- from until end
      x <- 0 until src.height
      if y >= 0 && y < src.height
    } yield {
      dst.update(x, y,  boxBlurKernel(src, x, y, radius))
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method
  def helper(fromToPairs: List[(Int, Int)]): Unit = {
    if (fromToPairs.isEmpty) Nil
    else {
      val pendingTasks = fromToPairs.tail.map(p => task {
        blur(src, dst, p._1, p._2, radius)
      })
      blur(src, dst, fromToPairs.head._1, fromToPairs.head._2, radius)
      pendingTasks.map(_.join())
    }
  }

    helper(fromToPairs(src.width, Math.min(1, numTasks)))
  }

  def fromToPairs(height: Int, numTasks: Int) = {
    val step = (height + 1) / numTasks
    val splittingPts = 0 to (height - 1) by step
    val fromLst = splittingPts.toList
    val toList = fromLst map (x => Math.min(x + step, height))
    fromLst zip toList
  }

}
