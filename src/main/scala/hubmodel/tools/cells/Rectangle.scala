package hubmodel.tools.cells

import java.util.concurrent.ThreadLocalRandom

import hubmodel.{Position, generateUUID}
import myscala.math.vector.Vector2D

class Rectangle(val name: String, C1: Position, C2: Position, C3: Position, C4: Position) extends Vertex {

  if (Vector(C1, C2, C3, C4).map(_.X).distinct.size != 2 || Vector(C1, C2, C3, C4).map(_.Y).distinct.size != 2){
    throw new IllegalArgumentException("Corners of rectangle do not make orthogonal rectangle")
  }

  val xMin: Double = Vector(C1, C2, C3, C4).map(_.X).min
  val yMin: Double = Vector(C1, C2, C3, C4).map(_.Y).min
  val xMax: Double = Vector(C1, C2, C3, C4).map(_.X).max
  val yMax: Double = Vector(C1, C2, C3, C4).map(_.Y).max

  protected var A: Position = Vector(C1, C2 ,C3, C4).find(p => p.X == xMin && p.Y == yMin).get
  protected var B: Position = Vector(C1, C2 ,C3, C4).find(p => p.X == xMax && p.Y == yMin).get
  protected var C: Position = Vector(C1, C2 ,C3, C4).find(p => p.X == xMax && p.Y == yMax).get
  protected var D: Position = Vector(C1, C2 ,C3, C4).find(p => p.X == xMin && p.Y == yMax).get

  if (A.Y != B.Y){
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
  var area: Double = (B - A).norm * (D - A).norm

  // collection of corners
  val corners: Vector[Position] = Vector(A, B, C, D)


  /** Is the point inside the vertex ?
    *
    * @param pos [[Position]] to check
    * @return boolean indicating if the point is inside the vertex.
    */
  def isInside(pos: Position): Boolean = {
    val AB: Position = B - A
    val BC: Position = C - B
    val AP: Position = pos - A
    val BP: Position = pos - B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }


  def uniformSamplePointInside: Position = {
    Vector2D(ThreadLocalRandom.current.nextDouble(A.X + 0.1, B.X - 0.1), ThreadLocalRandom.current.nextDouble(A.Y + 0.1, D.Y - 0.1))
  }

  def this(center: Position, hSide: Double, vSide: Double) {
    this(generateUUID, center + Vector2D(-0.5 * hSide, -0.5 * vSide), center + Vector2D(0.5 * hSide, -0.5 * vSide), center + Vector2D(0.5 * hSide, 0.5 * vSide), center + Vector2D(-0.5 * hSide, 0.5 * vSide))
  }

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: Rectangle => this.A == that.A && this.B == that.B && this.C == that.C && this.D == that.D
      case _ => false
    }

  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (this.A, this.B, this.C, this.D).##
  }

  override def toString: String = this.name

}