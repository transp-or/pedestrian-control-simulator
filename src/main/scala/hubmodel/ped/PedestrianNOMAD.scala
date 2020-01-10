package hubmodel.ped

import tools.Time
import tools.cells.{Rectangle, Vertex}
import hubmodel.{Acceleration, Direction, Position, Velocity}
import myscala.math.vector.{Vector2D, ZeroVector2D}

class PedestrianNOMAD(oZone: Vertex, dZone: Vertex, entryTime: Time, posO: Position, logFullHistory: Boolean, isTransfer: Boolean) extends PedestrianSim(oZone, dZone, entryTime, posO, logFullHistory, isTransfer) with WithGraphID {

  // List of pedestrians in the vicinity of this pedestrian
  var closePeds: Iterable[PedestrianNOMAD] = Vector()

  // Ghost pedestrian used to compute the fastest walking time
  val isInvisible: Boolean = false

  // Acceleration of this pedestrian
  var acceleration: Acceleration = new ZeroVector2D

  // Next position this pedestrian will move to. This is needed if parallel updates are used
  var nextPosition: Vector2D = this.currentPosition

  // Next velocity this pedestrian will have. This is needed if parallel updates are used
  var nextVelocity: Vector2D = new ZeroVector2D



  // ******************************************************************************************
  //                            AGENT-BASED PARAMETERS REQUIRED BY NOMAD
  // ******************************************************************************************

  // Isolation status with respect to pedestrians
  var isolationTypePed: Int = hubmodel.IN_COLLISION

  // Isolation status with respect to objects
  var isolationTypeObs: Int = hubmodel.IN_COLLISION

  // Isolation time with respect to objects
  var isolationTimeObs: Double = entryTime.value.toDouble

  // Isolation time with respect to pedestrians
  var isolationTimePed: Double = entryTime.value.toDouble

  // NOMAD parameter
  val AT: Double = 0.5

  // NOMAD parameter
  val infAreaMaxExtObs: Double = 1.0

  // NOMAD parameter
  var desiredDirection: Vector2D = new ZeroVector2D

  // NOMAD parameter
  val tau: Double = 0.15

  // NOMAD parameter
  val isStochastic: Boolean = true

  // NOMAD parameter
  val noise: Double = 0.0001

  // NOMAD parameter
  val getRadius: Double = this.r


  // ******************************************************************************************
  //                                      METHODS
  // ******************************************************************************************

  /** Not sure why this exists.. need to investigate
    *
    * @return
    */
  def isVariableStep: Boolean = { true }

  /** Determines whether this pedestrian is stuck somewhere. If the pedestrian hasn't moved more than 1mm in the last
    * 10 time steps the pedestrian is considered stuck.
    *
    * @return boolean indicating if the pedestrian is stuck
    */
  def isStuck: Boolean = {
    if (this._historyPositionUnsorted.size > 10) {
      (this._historyPositionUnsorted.dropRight(9).last._2.pos - this._historyPositionUnsorted.last._2.pos).norm < math.pow(10, -3)
    }
    else false
  }

  /** Computes the direction based on the current position and the target position
    *
    * @param pos  current position
    * @param goal target position
    * @return normalized directiondesiredDirection
    */
  protected def computeDesiredDirection(pos: Position, goal: Position): Direction = {
    (goal - pos) / (goal - pos).norm
  }

  /** Checks whether the speed is outside the feasible walking speed. If the pedestrian is stuck then make the
    * pedestrian go back to his previous destination.
    *
    */
  def updateDesiredDirection(): Unit = {

    if (this.isStuck) {
      this.nextZone = this.previousZone
      this.currentDestination = this.previousZone.uniformSamplePointInside
    }

    this.desiredDirection = computeDesiredDirection(this.currentPosition, this.currentDestination)
  }

  // ******************************************************************************************
  //                                   ALTERNATIVE CONSTRUCTORS
  // ******************************************************************************************

  /** Alternative constructor used to set the current position and velocity
    *
    * @param previousZone previous zone which the pedestrian visited
    * @param nextZone current target zone of the pedestrian
    * @param route current to to his final destination
    * @param dZone destination zone
    * @param entryTime entrance time
    * @param posO current position
    * @param velO current velocity
    * @param logFullHistory collect full history
    * @param isTransfer is the pedestrian a transfering passenger
    */
  def this(ID: String,
           previousZone: Vertex,
           nextZone: Vertex,
           route: List[Vertex],
           dZone: Vertex,
           entryTime: Time,
           posO: Position,
           velO: Velocity,
           logFullHistory: Boolean,
           isTransfer: Boolean,
           isolationData: (Int, Double, Int, Double)) {
    this(previousZone, dZone, entryTime, posO, logFullHistory, isTransfer)
    this.currentPosition = posO
    this.currentVelocity = velO
    this.route = route
    this.nextZone = nextZone
    this.previousZone = previousZone
    this.isolationTypePed = isolationData._1
    this.isolationTimePed = isolationData._2
    this.isolationTypeObs = isolationData._3
    this.isolationTimeObs = isolationData._4
    this._ID = ID

  }


  // ******************************************************************************************
  //                              METHODS FOR COPYING THE CLASS
  // ******************************************************************************************

  /** Creates a deep copy of the pedestrian. This will drop the previous history of the pedestrian (position, etc)
    *
    * @return deep copy of this pedestrian
    */
  def copyState(currentTime: => Time, logFullHistory: Boolean): PedestrianNOMAD = {
    new PedestrianNOMAD(
      this.ID,
      this.previousZone,
      this.nextZone,
      this.route,
      this.finalDestination,
      currentTime,
      this.currentPosition,
      this.currentVelocity,
      logFullHistory,
      this.isTransfer,
      (this.isolationTypePed, this.isolationTimePed, this.isolationTypeObs, this.isolationTimeObs))
  }

}
