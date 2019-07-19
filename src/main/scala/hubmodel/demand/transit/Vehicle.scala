package hubmodel.demand.transit

import hubmodel.supply.{NodeParent, StopID_New, TrainID_New}
import hubmodel.tools.Time

class Vehicle(val ID: TrainID_New, val trainType: String, val stop: StopID_New, val arr: Option[Time], val dep: Option[Time], val capacity: Int) {

  def alightingPassengers: scala.collection.Seq[NodeParent] = this._alightingPassengers

  private val _alightingPassengers: collection.mutable.ArrayBuffer[NodeParent] = collection.mutable.ArrayBuffer()

  def addAlightingPassenger(dZone: NodeParent): Unit = {
    this._alightingPassengers.append(dZone)
  }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Vehicle]

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: Train => super.equals() && that.canEqual(this) && this.stop == that.stop && this.arr == that.arr && this.dep == that.dep
      case _ => false
    }

  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (super.hashCode, this.trainType, this.arr, this.dep).##
  }

}
