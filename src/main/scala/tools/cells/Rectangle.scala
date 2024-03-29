package tools.cells

import java.util.concurrent.ThreadLocalRandom

import hubmodel.{Position, generateUUID}
import myscala.math.vector.Vector2D

class Rectangle(val name: String, C1: Position, C2: Position, C3: Position, C4: Position, val isOD: Boolean, genRate: Option[Double]) extends Vertex {

  /** Constructor with the name as an argument. The name is replaced with the ID
    *
    * @param C1 first corner
    * @param C2 second corner
    * @param C3 third corner
    * @param C4 fourth corner
    * @param isOD origin or destination ?
    * @param genRate maximal peddestrian generation rate in this cell
    */
  def this(C1: Position, C2: Position, C3: Position, C4: Position, isOD: Boolean, genRate: Option[Double] = None) {
    this("", C1, C2, C3, C4, isOD, genRate)
  }

  @deprecated
def this(data: Array[Double]) {
    this(data(0).toString, new Position(data(2), data(3)), new Position(data(4), data(5)), new Position(data(6), data(7)), new Position(data(8), data(9)), false, None)
  }

  if (Vector(C1, C2, C3, C4).map(_.X).distinct.size != 2 || Vector(C1, C2, C3, C4).map(_.Y).distinct.size != 2) {
    throw new IllegalArgumentException("Corners of rectangle do not make orthogonal rectangle ! + id=" + name)
  }

  val xMin: Double = Vector(C1, C2, C3, C4).map(_.X).min
  val yMin: Double = Vector(C1, C2, C3, C4).map(_.Y).min
  val xMax: Double = Vector(C1, C2, C3, C4).map(_.X).max
  val yMax: Double = Vector(C1, C2, C3, C4).map(_.Y).max

  protected var A: Position = Vector(C1, C2, C3, C4).find(p => p.X == xMin && p.Y == yMin).get
  protected var B: Position = Vector(C1, C2, C3, C4).find(p => p.X == xMax && p.Y == yMin).get
  protected var C: Position = Vector(C1, C2, C3, C4).find(p => p.X == xMax && p.Y == yMax).get
  protected var D: Position = Vector(C1, C2, C3, C4).find(p => p.X == xMin && p.Y == yMax).get

  if (A.Y != B.Y) {
    throw new IllegalArgumentException("First two points don't have same Y coordinates for rectangle \"" + this.name + "\", A=" + A + ", B=" + B)
  } else if (C.Y != D.Y) {
    throw new IllegalArgumentException("Last two points don't have same Y coordinates for rectangle \"" + this.name + "\"")
  } else if (A.X != D.X) {
    throw new IllegalArgumentException("Left most points don't have same X coordinates for rectangle \"" + this.name + "\"")
  } else if (B.X != C.X) {
    throw new IllegalArgumentException("Right most two points don't have same X coordinates for rectangle \"" + this.name + "\"")
  }

  // center of the rectangle
  var center: Position = A + (B - A) * 0.5 + (D - A) * 0.5

  // area of the associated zone
  val area: Double = (B - A).norm * (D - A).norm

  // collection of corners
  val corners: Vector[Position] = Vector(A, B, C, D)

  // width
  val width: Double = (this.B-this.A).norm

  // height
  val height: Double = (this.C-this.B).norm

  // Rate in pedestrians/second at which to generate pedestrians inside this cell.
  val generationRate: Double = if (genRate.isDefined) {
    genRate.get
  } else {
    10.0
  }


  /** Is the point inside the vertex ?
  *
  * @param pos [[Position]] to check
  * @return boolean indicating if the point is inside the vertex.
  */
  def isInside(pos: Position, wide: Boolean): Boolean = {
    val AB: Position = (B + new Position(-0.1, 0.1)) - (A + new Position(0.1, 0.1))
    val BC: Position = (C + new Position(-0.1, -0.1)) - (B + new Position(-0.1, 0.1))
    val AP: Position = pos - (A + new Position(0.1, 0.1))
    val BP: Position = pos - (B + new Position(-0.1, 0.1))
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

  /**
  * Generate a point inside the rectangle with uniform probabilities in both directions
  *
  * @return
  */
  def uniformSamplePointInside: Position = {
    Vector2D(ThreadLocalRandom.current.nextDouble(A.X + 0.1, B.X - 0.1), ThreadLocalRandom.current.nextDouble(A.Y + 0.1, D.Y - 0.1))
  }

  def this(center: Position, hSide: Double, vSide: Double, od: Boolean, genRate: Option[Double]) {
    this(generateUUID, center + Vector2D(-0.5 * hSide, -0.5 * vSide), center + Vector2D(0.5 * hSide, -0.5 * vSide), center + Vector2D(0.5 * hSide, 0.5 * vSide), center + Vector2D(-0.5 * hSide, 0.5 * vSide), od, genRate)
  }

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
  *
  * @param other another object to test equality for
  * @return boolean indicating if the two objects are the same
  */
  override def equals(other: Any): Boolean =
    other match {
      case that: Rectangle if that.canEqual(this) => this.A == that.A && this.B == that.B && this.C == that.C && this.D == that.D
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Rectangle]


  /** Definition of equality.
  *
  * @return Int representing the object
  */
  override def hashCode: Int = {
    (this.A, this.B, this.C, this.D).##
  }

  override def toString: String = this.name

  type T =  Rectangle

  override def deepCopy: T = new Rectangle(this.name, this.C1, this.C2, this.C3, this.C4, this.isOD, this.genRate)
}