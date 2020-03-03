package hubmodel.control.flowgate

import hubmodel.Position
import hubmodel.control.{ControlDeviceComponent, FunctionalForm, Measurement, Output}
import tools.cells.Vertex

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
class FlowGateFunctional[T <: Measurement, U <: Output](startVertex: Vertex, endVertex: Vertex, start: Position, end: Position, ma: String, val functionalForm: FunctionalForm[T, U]) extends FlowGate(startVertex, endVertex, start, end, ma) with ControlDeviceComponent {

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: FlowGateFunctional[T, U] => super.equals() && that.canEqual(this) && this.start == that.start && this.end == that.end
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FlowGateFunctional[_, _]]

  override def toString: String = "FlowGateFunctional. ID=" + this.ID + ", o=" + startVertex + ", d=" + endVertex

  /**
    * Copies this flow gate but changed the function linking the density to the controlled flow.
    *
    * @param newFunctionForm new functional form
    * @return copy of the flow gate
    */
  def deepCopy[V <: Measurement, W <: Output](newFunctionalForm: FunctionalForm[V, W]): FlowGateFunctional[V, W] = {
    new FlowGateFunctional(this.startVertex, this.endVertex, this.start, this.end, this.monitoredArea, newFunctionalForm)
  }
}

