


package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }


  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x_in: Int, y_in: Int, radius: Int): RGBA = {
    // TODO implement using while loops

    var r_sum,g_sum,b_sum,a_sum,num_sq = 0
    var x_min = clamp(x_in-radius: Int, 0 , src.width-1)
    var x_max = clamp(x_in+radius: Int, 0 , src.width-1)
    var y_min = clamp(y_in-radius: Int, 0 , src.height-1)
    var y_max = clamp(y_in+radius: Int, 0 , src.height-1)

    def add_rgba(somecolor:RGBA): Unit ={
      r_sum = r_sum + red(somecolor)
      g_sum = g_sum + green(somecolor)
      b_sum = b_sum + blue(somecolor)
      a_sum = a_sum + alpha(somecolor)
    }
    var x = x_min
    var y = y_min
    while(y<=y_max){
      x = x_min
      while(x<=x_max){
        add_rgba(src.apply(x,y))
        num_sq = num_sq + 1
        x = x+1
      }
      y = y+1
    }
    def get_colour(red:Int,blue:Int,green:Int,alpha:Int,num_cells:Int): RGBA ={
      r_sum = r_sum/num_sq
      g_sum = g_sum/num_sq
      b_sum = b_sum/num_sq
      a_sum = a_sum/num_sq
      rgba(r_sum,g_sum,b_sum,a_sum)
    }
    get_colour(r_sum,g_sum,b_sum,a_sum,num_sq)

  }

}
