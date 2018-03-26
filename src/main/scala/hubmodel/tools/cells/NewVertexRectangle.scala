package hubmodel.tools.cells

import java.util.concurrent.ThreadLocalRandom

import hubmodel.{Position, generateUUID}
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
class NewVertexRectangle(val name: String, val A: Position, val B: Position, val C: Position, val D: Position) extends RectangularVertexTrait {

  def this(center: Position, hSide: Double, vSide: Double) {
    this(generateUUID, center + Vector2D(-0.5*hSide, -0.5*vSide), center + Vector2D(0.5*hSide, -0.5*vSide), center + Vector2D(0.5*hSide, 0.5*vSide), center + Vector2D(-0.5*hSide, 0.5*vSide))
  }

  /** Equality based on the ID and not the positions
    *
    * @param other [[RectangularVertexTrait]] to which we want to compare to
    * @return
    */
  /*def equalsID(other: Any): Boolean = {
    other match {
      case that: VertexRectangle => this.ID == that.ID
      case _ => false
    }
  }*/

  /** Checks whether another object equals this one by comparing the positions associated to the vertex
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: RectangularVertexTrait => this.A == that.A && this.B == that.B && this.C == that.C && this.D == that.D
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
