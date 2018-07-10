package hubmodel.supply.graph

import hubmodel.Position
import hubmodel.tools.cells.Rectangle

/** Extension of [[hubmodel.supply.MyEdgeWithGate]] for the usage of "flow gates". The gates control the
  * flow of pedestrians passing through them.
  *
  * TODO: A more realistic approach for the modeling of gates could be used to determine the maximum capacity.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param start       one end of the gate
  * @param end         other end of the gate
  */
class FlowGateFunctional(startVertex: Rectangle, endVertex: Rectangle, start: Position, end: Position, ma: String, val functionalForm: Double => Double) extends FlowGate(startVertex, endVertex, start, end, ma) {

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: FlowGateFunctional => super.equals() && that.canEqual(this) && this.start == that.start && this.end == that.end
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FlowGateFunctional]

  override def toString: String = "FlowGateFunctional: o: " + startVertex + ", d:" + endVertex
}