package hubmodel.tools.cells

import java.util.concurrent.ThreadLocalRandom

import hubmodel.Position
import hubmodel.tools.Time
import myscala.math.vector.Vector2D


/** Representation of a vertex, or cell. This is used for representing vertices in the graph specification
  * and can be extend to manage any sort cell or zone. This specification is that of rectangles with vertical
  * and horizontal sides. For more advanced shapes, this class should be overriden. If this is done, the methods
  * for generating points inside the cells must also be overriden.
  *
  * @param name name for humans
  * @param A    bottom left
  * @param B    bottom right
  * @param C    top right
  * @param D    top left
  */
class RectangleModifiable(name: String,
                          private val __A: (Position, Position),
                          private val __B: (Position, Position),
                          private val __C: (Position, Position),
                          private val __D: (Position, Position),
                          od: Boolean,
                          genRate: Option[Double]) extends Rectangle(name, (__A._1 + __A._2) * 0.5, (__B._1 + __B._2) * 0.5, (__C._1 + __C._2) * 0.5, (__D._1 + __D._2) * 0.5, od, genRate) {

  A = (__A._1 + __A._2) * 0.5
  B = (__B._1 + __B._2) * 0.5
  C = (__C._1 + __C._2) * 0.5
  D = (__D._1 + __D._2) * 0.5

  private var ATarget: Position = A
  private var BTarget: Position = B
  private var CTarget: Position = C
  private var DTarget: Position = D

  def getCornersBoundaries: ((Position, Position), (Position, Position), (Position, Position), (Position, Position)) = {
    (this.__A, this.__B, this.__C, this.__D)
  }

  private val speed: Double = 0.5 // m/s fixed in a arbitrary manner

  def moveRectangle(dt: Time): Unit = {
    if ((this.A - this.ATarget).norm > 0.0) this.A = this.A + (this.ATarget - this.A).normalized * (speed * dt.value.toDouble)

    if ((this.B - this.BTarget).norm > 0.0) this.B = this.B + (this.BTarget - this.B).normalized * (speed * dt.value.toDouble)

    if ((this.C - this.CTarget).norm > 0.0) this.C = this.C + (this.CTarget - this.C).normalized * (speed * dt.value.toDouble)

    if ((this.D - this.DTarget).norm > 0.0) this.D = this.D + (this.DTarget - this.D).normalized * (speed * dt.value.toDouble)
  }

  def setTargetPosition(fraction: Double): Unit = {
    this.ATarget = this.__A._1 + (this.__A._2 - this.__A._1) * fraction
    this.BTarget = this.__B._1 + (this.__B._2 - this.__B._1) * fraction
    this.CTarget = this.__C._1 + (this.__C._2 - this.__C._1) * fraction
    this.DTarget = this.__D._1 + (this.__D._2 - this.__D._1) * fraction
  }

  override def uniformSamplePointInside: Position = {
    Vector2D(ThreadLocalRandom.current.nextDouble(this.A.X + 0.1, this.B.X - 0.1), ThreadLocalRandom.current.nextDouble(this.A.Y + 0.1, this.D.Y - 0.1))
  }

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: RectangleModifiable => this.__A == that.__A && this.__B == that.__B && this.__C == that.__C && this.__D == that.__D
      case _ => false
    }

  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (this.__A, this.__B, this.__C, this.__D).##
  }

  override def toString: String = this.name //+ ", " + this.A + ", " + this.B + ", " + this.C + ", " + this.D


  override def clone(): RectangleModifiable = new RectangleModifiable(
    this.name, this.__A, this.__B, this.__C, this.__D, this.isOD, Some(this.generationRate)
  )

}
