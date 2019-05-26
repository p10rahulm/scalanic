
import barneshut.conctrees._
import common._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX: Float, centerY: Float, size: Float, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = (nw.centerX + ne.centerX + sw.centerX + se.centerX) / 4
    val centerY: Float = (nw.centerY + ne.centerY + sw.centerY + se.centerY) / 4
    val size: Float = nw.size + ne.size + sw.size + se.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = {
      if (mass != 0)
        (nw.massX * nw.mass + ne.massX * ne.mass + sw.massX * sw.mass + se.massX * se.mass) / mass
      else
        centerX
    }
    val massY: Float = {
      if (mass != 0)
        (nw.massY * nw.mass + ne.massY * ne.mass + sw.massY * sw.mass + se.massY * se.mass) / mass
      else
        centerY
    }
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if(b.x>centerX && b.y > centerY) Fork(nw, ne, sw, se.insert(b))
      else if(b.x<centerX && b.y > centerY) Fork(nw, ne, sw.insert(b), se)
      else if(b.x>centerX && b.y < centerY) Fork(nw, ne.insert(b), sw, se)
      else Fork(nw.insert(b), ne, sw, se)
    }

  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val mass:Float = bodies.map(body=>body.mass).sum
    val massX:Float = if(mass != 0) bodies.map(body=>body.x*body.mass).sum/mass else centerX
    val massY:Float = if(mass != 0) bodies.map(body=>body.y*body.mass).sum/mass else centerY
    val total: Int = bodies.length

    def insert(b: Body): Quad = {
      val new_bodies = bodies :+ b //append to seq in scala
      if (size <= minimumSize)
        Leaf(centerX, centerY, size, new_bodies)
      else {
        val (left_x, right_x, top_y, bottom_y) = (centerX - size / 4, centerX + size / 4, centerY - size / 4, centerY + size / 4)
        val nw = Empty(left_x, top_y, size / 2)
        val sw = Empty(left_x, bottom_y, size / 2)
        val ne = Empty(right_x, top_y, size / 2)
        val se = Empty(right_x, bottom_y, size / 2)
        val newfork = Fork(nw, ne, sw, se)
        new_bodies.foldRight(newfork)((x,y)=>y.insert(x))
      }
    }

  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) => None
          // no force
        case Leaf(_, _, _, bodies) => bodies.foreach(body=>addForce(body.mass,body.x,body.y))
          // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
          if (quad.size / distance(x, y, quad.centerX, quad.centerY) < theta){
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
          // see if node is far enough from the body,
          // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val x_bucket_num: Int = Math.min(Math.max(((b.x - boundaries.minX) / (boundaries.width / sectorPrecision)).toInt,0),sectorPrecision-1)
      val y_bucket_num: Int = Math.min(Math.max(((b.y - boundaries.minY) / (boundaries.height/ sectorPrecision)).toInt,0),sectorPrecision-1)
      this.apply(x_bucket_num,y_bucket_num) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- 0 until matrix.length){
        this.matrix.update(i, this.matrix(i).combine(that.matrix(i)))
      }
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
