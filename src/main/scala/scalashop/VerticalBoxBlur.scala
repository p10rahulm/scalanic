package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

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
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    var column:Int = Math.max(from,0)
    var row:Int = 0
    while(column<src.width && column<end){
      row = 0
      while(row<src.height){
        dst.update(column,row,boxBlurKernel(src, column,row,radius))
        row = row +1
      }
      column = column+1
    }
  }

  def blur11(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    var column:Int = from
    var row:Int = 0
    while(column<src.width && column<end){
      row = 0
      while(row<src.height){
        var i = -radius
        var j = -radius
        var kernel_rgba = boxBlurKernel(src, column,row,radius)
        while(i<=radius){
          while(j<=radius){
            dst.update(clamp(column+i,0,src.width-1),clamp(row+j,0,src.height-1),kernel_rgba)
            j=j+1
          }
          i=i+1
        }

//        dst(column,row) = boxBlurKernel(src, column,row,radius)
        row = row +2*radius+1
      }
      column = column+2*radius+1
    }
  }


  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val width_of_each_strip = Math.max(src.width / numTasks,1) //min 1 width
    val tasks = (0 until src.width by width_of_each_strip).map(column => task(blur(src,dst,column,column+width_of_each_strip,radius)))
    tasks.map(x => x.join)
  }

}
