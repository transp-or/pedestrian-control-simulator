package hubmodel

import hubmodel.input.infrastructure.NodeID

/**
  * Parent trait for different implementation of pedestrians. This can make functions more generic.
  */
trait PedestrianTrait {

  /** unique identifier */
  val ID: String = generateUUID

  /** origin zone */
  def oZone: NodeID

  /** destination zone */
  def dZone: NodeID

  /** current position */
  def currentPositionNew: NewBetterPosition2D

  /** old current position */
  @deprecated
  def currentPosition: Position

  /** total travel time */
  def travelTime: Time

  /** total distance travelled */
  def travelDistance: Double

  /** free flow velocity */
  def freeFlowVel: Double

  /** entry time into the system */
  def entryTime: Time

  /** exit time from the system */
  def exitTime: Time

  /** returns the OD pair as a tuple */
  def getODTuple: (NodeID, NodeID) = (oZone, dZone)

  /** return the OD pair as a Vector */
  def getODVec: Vector[NodeID] = Vector(oZone, dZone)

  /** has reached destination and has exited simulation */
  var completed: Boolean = false

  /** needs to be specified for each implementation */
  override def toString: String

  /** Returns the bounded velocity
    *
    * @param desiredVel initial velocity
    * @return bounded velocity
    */
  def boundVelocity(desiredVel: Velocity): Velocity = {
    if (breeze.linalg.norm(desiredVel) <= (1.3 * 1.34)) desiredVel
    else ((1.3 * 1.34) / breeze.linalg.norm(desiredVel)) * desiredVel
  }

  def boundVelocity(desiredVel: NewBetterVelocity2D): NewBetterVelocity2D = {
    if (desiredVel.norm <= (1.3 * 1.34)) desiredVel
    else desiredVel * ((1.3 * 1.34) / desiredVel.norm)
  }

  def toVisioSafeFormat(refDate: String = "2013,1,1"): String

  /** method needed to guarantee equality is tested correctly */
  def canEqual(a: Any): Boolean = a.isInstanceOf[PedestrianTrait]

  /** equality between this pedestrian and another object.
    *
    * @param a the object to test equality with
    * @return whether the object equals this pedestrian
    */
  override def equals(a: Any): Boolean =
    a match {
      case that: PedestrianTrait => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  /** Definition of the propoerty used for testing equality.
    * As each pedestrian has a unique ID assigned, this can be used for testing equality.
    *
    * @return the UUID transformed into hash code
    */
  override def hashCode: Int = {
    ID.hashCode
  }
}
