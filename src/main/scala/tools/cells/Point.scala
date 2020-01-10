package tools.cells

import hubmodel.Position

/** Vertex without any associated zone. This is simply a coordinate point.
  * - [[isInside()]] always returns false.
  * - generationRate is set to 0 as no pedetrian can appear here.
  * - isOD is set to false.
  *
  * @param name human readible name given to the coordinate
  * @param coordinate position
  *
  */
class Point(val name: String, coordinate: Position) extends Vertex {

  val isOD: Boolean = false

  val area: Double = 0.0

  def uniformSamplePointInside: Position = coordinate

  val center: Position = coordinate

  val generationRate: Double = 0.0

  val corners: Vector[Position] = Vector(coordinate)

  def isInside(p: Position): Boolean = false

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  override def hashCode: Int = (this.coordinate).##

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: Point if that.canEqual(this) => this.hashCode == that.hashCode
      case _ => false
    }

  type T = Point

  def deepCopy: T = new Point(this.name, this.coordinate)
}
