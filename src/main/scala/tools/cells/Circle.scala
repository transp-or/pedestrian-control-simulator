package tools.cells

import java.util.concurrent.ThreadLocalRandom

import hubmodel.Position

class Circle(val name: String, val center: Position, radius: Double, val isOD: Boolean, genRate: Option[Double]) extends Vertex {

  // Area of the circle
  val area: Double = 0.5 * this.radius * this.radius * math.Pi

  /** For generating points uniformly inside a circle, see https://programming.guide/random-point-within-circle.html
    *
    * @return
    */
  def uniformSamplePointInside: Position = {

    // angle
    val a = ThreadLocalRandom.current().nextDouble() * 2 * math.Pi

    // radius
    val r = 0.1 * this.radius * math.sqrt(ThreadLocalRandom.current().nextDouble())

    // cartesian coordinates
    this.center + new Position(r * math.cos(a), r * math.sin(a))
  }

  /** Needs fixing
    * TODO improve this, main problem is for sorting out videos.
    */
  val corners: Vector[Position] = Vector(this.center + new Position(this.radius, 0.0),
    this.center + new Position(0.0, this.radius),
    this.center + new Position(-this.radius, 0.0),
    this.center + new Position(0.0, - this.radius ))

  def isInside(p: Position, wide: Boolean): Boolean = {
    if (wide) {
      (p - this.center).norm <= 3 *this.radius
    } else {
      (p - this.center).norm <= this.radius
    }
  }

  // Rate in pedestrians/second at which to generate pedestrians inside this cell.
  val generationRate: Double = if (genRate.isDefined) {
    genRate.get
  } else {
    20.0
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Circle]

  override def hashCode: Int = (this.center, this.radius).##

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: Circle if that.canEqual(this) => this.hashCode == that.hashCode
      case _ => false
    }

  type T =  Circle

  override def deepCopy: T = new Circle(this.name, this.center, this.radius, this.isOD, this.genRate)

}
