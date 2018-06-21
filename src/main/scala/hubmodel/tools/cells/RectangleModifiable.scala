package hubmodel.tools.cells

import hubmodel.Position


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
                          private val __D: (Position, Position)) extends Rectangle(name, (__A._1 + __A._2) * 0.5, (__B._1 + __B._2) * 0.5, (__C._1 + __C._2) * 0.5, (__D._1 + __D._2) * 0.5) {

  A = (__A._1 + __A._2) * 0.5
  B = (__B._1 + __B._2) * 0.5
  C = (__C._1 + __C._2) * 0.5
  D = (__D._1 + __D._2) * 0.5

  def getCornersBoundaries: ((Position, Position), (Position, Position), (Position, Position), (Position, Position)) = {
    (this.__A, this.__B, this.__C, this.__D)
  }

  def updatePositions(fraction: Double): Unit = {
    this.A = this.__A._1 + (this.__A._2 - this.__A._1) * fraction
    this.B = this.__B._1 + (this.__B._2 - this.__B._1) * fraction
    this.C = this.__C._1 + (this.__C._2 - this.__C._1) * fraction
    this.D = this.__D._1 + (this.__D._2 - this.__D._1) * fraction
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

  override def toString: String = this.name


}