package hubmodel.ped

import hubmodel._
import hubmodel.mvmtmodels.NOMAD.getClosestPoint
import hubmodel.supply.continuous.Wall
import hubmodel.tools.cells.Rectangle


/**
  * Parent trait for different implementation of pedestrians. This can make functions more generic.
  */
trait PedestrianTrait {

  // ******************************************************************************************
  //                                MEMBERS REQUIRED BY THE SIMULATION
  // ******************************************************************************************

  /** unique identifier */
  val ID: String = generateUUID

  /** origin zone */
  def origin: Rectangle

  /** destination zone */
  def finalDestination: Rectangle

  /** current position */
  def currentPosition: Position

  /** previous position at the previous major timestep (sf_dt) */
  var previousMajorPosition: Position = _

  /** total travel time */
  def travelTime: Time

  /** total distance travelled */
  def travelDistance: Double

  /** free flow velocity */
  def freeFlowVel: Double

  /** entry time into the system */
  def entryTime: Time

  /** exit time from the system */
  var exitTime: Time

  /** has reached destination and has exited simulation */
  var reachedDestination: Boolean = false


  /**
    * List of closest walls to compute interactions with. Avoids search all walls every time.
    */
  var closeWalls: Iterable[Wall] = Vector()

  def updateClosestWalls(walls: Iterable[Wall]): Unit = {
    closeWalls = walls.filter(w => (this.currentPosition - getClosestPoint(this.currentPosition, w)).norm < DISTANCE_TO_CLOSE_WALLS)
  }

  /**
    * Upper bounds the velocity with a value of 1.3*1.34.
    * This value is taken from the original paper on the Social Force Model.
    *
    * @param desiredVel initial velocity
    * @return bounded velocity
    */
  def boundVelocity(desiredVel: Velocity): Velocity = {
    if (desiredVel.norm <= (1.3 * 1.34)) desiredVel
    else desiredVel * ((1.3 * 1.34) / desiredVel.norm)
  }

  def updatePositionHistory(t: Time): Unit

  // ******************************************************************************************
  //                    FUNCTIONS FOR PRINTING THE PEDESTRIAN
  // ******************************************************************************************

  /** needs to be specified for each implementation */
  override def toString: String

  /** Writes the trajectory of the pedestrian in the same format as VisioSafe
    *
    * @param refDate default parameter set to 2013-01-01
    * @return String with full trajectory
    */
  def toVisioSafeFormat(refDate: String = "2013,1,1"): String

  // ******************************************************************************************
  //                              FUNCTIONS FOR TESTING EQUALITY
  // ******************************************************************************************

  /**
    * method needed to guarantee equality is tested correctly
    *
    * @param a object to compare to
    * @return is the comparison allowed
    */
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

  /**
    * Definition of the property used for testing equality.
    * As each pedestrian has a unique ID assigned, this can be used for testing equality.
    *
    * @return the UUID transformed into hash code
    */
  override def hashCode: Int = {
    ID.hashCode
  }
}
