package hubmodel.mgmt.flowgate

import hubmodel.Position
import hubmodel.mgmt.ControlDeviceComponent
import hubmodel.supply.graph.MyEdgeWithGate
import tools.cells.{Rectangle, Vertex}

/** Object to model gates controlling the flow of pedestrians
  *
  * @param o     vertex "before" the gate
  * @param d     vertex "after" the gate
  * @param start one end of the gate (used for visualization)
  * @param end   other end of the gate (used for visualization)
  */
@deprecated("use flow gates instead")
class BinaryGate(o: Vertex,
                 d: Vertex,
                 start: Position,
                 end: Position,
                 ma: String) extends MyEdgeWithGate(o, d, start, end, ma) with ControlDeviceComponent {


  //var flowRate: Double = Double.MaxValue

  // On creation, all gates are open
  var isOpen: Boolean = true

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: BinaryGate => super.equals() && that.canEqual(this) && this.start == that.start && this.end == that.end
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[BinaryGate]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (super.hashCode, this.start, this.end).##
  }

  override def deepCopy: BinaryGate = new BinaryGate(
    this.o, this.d, this.start, this.end, this.ma
  )
}
