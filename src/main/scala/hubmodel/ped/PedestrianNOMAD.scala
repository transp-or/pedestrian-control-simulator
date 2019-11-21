package hubmodel.ped

import hubmodel.tools.Time
import hubmodel.tools.cells.Rectangle
import hubmodel.{Acceleration, Direction, Position}
import myscala.math.vector.{Vector2D, ZeroVector2D}

class PedestrianNOMAD(oZone: Rectangle, dZone: Rectangle, entryTime: Time, posO: Position, logFullHistory: Boolean, isTransfer: Boolean) extends PedestrianSim(oZone, dZone, entryTime, posO, logFullHistory, isTransfer) with WithGraphID {

  def isVariableStep: Boolean = {
    true
  }

  var closePeds: Iterable[PedestrianNOMAD] = Vector()


  var isolationTypePed: Int = hubmodel.IN_COLLISION
  var isolationTypeObs: Int = hubmodel.IN_COLLISION
  val isInvisible: Boolean = false
  var isolationTimeObs: Double = entryTime.value.toDouble
  var isolationTimePed: Double = entryTime.value.toDouble
  val AT: Double = 0.5
  val infAreaMaxExtObs: Double = 1.0
  var desiredDirection: Vector2D = new ZeroVector2D
  val tau: Double = 0.15
  val isStochastic: Boolean = true
  val noise: Double = 0.0001
  var acceleration: Acceleration = new ZeroVector2D
  var nextPosition: Vector2D = this.currentPosition
  var nextVelocity: Vector2D = new ZeroVector2D
  //val aw: Double = 10
  //val s0: Double = 0.26
  val getRadius: Double = this.r

  def isStuck: Boolean = {
    if (this._historyPositionUnsorted.size > 10) {
      (this._historyPositionUnsorted.dropRight(9).last._2.pos - this._historyPositionUnsorted.last._2.pos).norm < math.pow(10, -3)
    }
    else false
  }

  /*def updatePreviousPositionAndSpeed(t: Time): Unit = {
    this.updatePositionHistory(t)
  }*/

  /** Computes the direction based on the current position and the target position
    *
    * @param pos  current position
    * @param goal target position
    * @return normalized directiondesiredDirection
    */
  protected def computeDesiredDirection(pos: Position, goal: Position): Direction = {
    (goal - pos) / (goal - pos).norm
  }

  protected def computePathFollowingComponent(p: PedestrianSim): Acceleration = {
    val tau: Double = 0.62
    (computeDesiredDirection(p.currentPosition, p.currentDestination) * p.desiredWalkingSpeed - p.currentVelocity) / tau
  }

  def updateDesiredSpeed(): Unit = {
    if (this.isStuck) {
      //println("ped " + this.ID + " is stuck")
      this.nextZone = this.previousZone
      this.currentDestination = this.previousZone.uniformSamplePointInside
    }

    this.desiredDirection = computeDesiredDirection(this.currentPosition, this.currentDestination) //computePathFollowingComponent(this).normalized
  }
}
