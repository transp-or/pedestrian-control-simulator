package hubmodel.mvmtmodels

import breeze.linalg.max
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel._
import hubmodel.ped.PedestrianSim
import hubmodel.supply.continuous.Wall
import myscala.math.vector.{Vector2D, ZeroVector2D}

class NOMADModel(sim: SFGraphSimulator) extends Action {


  /** Computes the direction based on the current position and the target position
    *
    * @param pos  current position
    * @param goal target position
    * @return normalized direction
    */
  protected def computeDesiredDirection(pos: Position, goal: Position): Direction = {
    (goal - pos) / (goal - pos).norm
  }

  protected def computePathFollowingComponent(p: PedestrianSim): Acceleration = {
    val tau: Double = 0.62
    (computeDesiredDirection(p.currentPosition, p.currentDestination) * p.freeFlowVel - p.currentVelocity) / tau
  }


  /** Find the projection from of a point onto the wall
    *
    * @param pos point to project
    * @param w   [[Wall]] onto which the point must be projectec
    * @return The projected point
    */
  protected def computeProjection(pos: Position, w: Wall): Position = {
    val AP: Position = pos - w.startPoint
    val AB: Position = w.endPoint - w.startPoint
    AB * ((AB dot AP) / (AB dot AB)) + w.startPoint
  }

  /** True of the point is on the wall, false if not
    *
    * @param point point to check
    * @param w     wall
    * @return boolean whether indictating if the point is on the wall
    */
  protected def isOnWall(point: Position, w: Wall): Boolean = {
    ((w.startPoint.X <= point.X && point.X <= w.endPoint.X) && (w.startPoint.Y <= point.Y && point.Y <= w.endPoint.Y)) || ((w.endPoint.X <= point.X && point.X <= w.startPoint.X) && (w.endPoint.Y <= point.Y && point.Y <= w.startPoint.Y))
  }

  /** Finds the closest end point of a wall if the point is not on the wall
    *
    * @param point point which is not on the wall
    * @param w     wall to finds end from
    * @return closest end point of the wall to the point
    */
  protected def getClosestEndPoint(point: Position, w: Wall): Position = {
    if ((w.startPoint - point).norm < (w.endPoint - point).norm) {w.startPoint}
    else {w.endPoint}
  }

  /** Find the point used to compute the repulsive effects from the wall onto a pedestrian.
    *
    * @param pos Position of the pedestrian
    * @param w   wall to calculate repulsion from
    * @return position used to calculate repulsion
    */
  protected def getClosestPoint(pos: Position, w: Wall): Position = {
    val proj: Position = computeProjection(pos, w)
    if (!isOnWall(proj, w)) getClosestEndPoint(pos, w)
    else proj
  }

  protected def obstacleInteractionAcceleration(p: PedestrianSim, w: Wall): Acceleration = {
    val shy: Double = 1.0 // whats is the value of this parameter ?
    val aW: Double = -1.0 * 10.0 // multiplication by -1 to take into account the negative sign
    val p2wall: Vector2D = getClosestPoint(p.currentPosition, w) - p.currentPosition
    val d: Double = p2wall.norm
    if (d > shy) new ZeroVector2D
    else if (d <= 0.5 * shy) p2wall.normalized * aW
    else p2wall.normalized * 2.0 * (1.0 - d / shy) * aW
  }

  private def projectOntoMe(me: Vector2D, that: Vector2D): Vector2D = {
    me * ((me dot that) / (me.norm * me.norm))
  }

  private def computePedestrianInteraction(p: PedestrianSim, that: PedestrianSim): Acceleration = {
    val a0: Double = -1.0 * 2.02 // multiplication by -1 to take into account the negative sign
    val r0: Double = 1.24
    val a1: Double = -1.0 * 7.08 // multiplication by -1 to take into account the negative sign
    val r1: Double = 0.69
    val anticipationTime: Double = 0.53

    val (pPos, thatPos) = if (p.currentVelocity.dot(that.currentVelocity) > 0.0) { // don't need to normalize as sign is of interest
      (p.currentPosition, that.currentPosition)
    } else {
      (p.currentPosition + p.currentVelocity * anticipationTime, that.currentPosition + that.currentVelocity * anticipationTime)
    }
    val vecBetweenPeds: Vector2D = thatPos - pPos
    val distanceBetweenPeds: Double = max(vecBetweenPeds.norm, 0.0*(p.r + that.r))

    //if (p.currentVelocityNew.norm > 0.0 && ((p.currentPositionNew - that.currentPositionNew).norm <= (p.r + that.r) )) {
    val delta: Double = (p.r + that.r) - (p.currentPosition - that.currentPosition).norm
    val k0: Double = -1000.0
    val k1: Double = 1000.0

    val smoothingPar1: Double = -0.5
    val smoothingPar2: Double = 10.0
    val smoothingPar3: Double = 1.0


    val tmpOrthogonalDirectionCloseRange: Direction = new Direction((that.currentPosition - p.currentPosition).Y, (that.currentPosition - p.currentPosition).X * -1.0).normalized
    val orthogonalDirectionCloseRange: Direction = if (tmpOrthogonalDirectionCloseRange.dot(p.currentVelocity) < 0.0) {tmpOrthogonalDirectionCloseRange * -1.0} else {tmpOrthogonalDirectionCloseRange}

    //(that.currentPositionNew - p.currentPositionNew).normalize * k0 * delta + orthogonalDirection*(that.currentVelocityNew.norm - p.currentVelocityNew.norm) * k1 * delta
      if (p.currentVelocity.norm > 0.0 && p.currentVelocity.dot(vecBetweenPeds) > 0.0) {
        val orthogonalDirection: Direction = projectOntoMe(new Direction(p.currentVelocity.Y, p.currentVelocity.X * -1.0).normalized, vecBetweenPeds).normalized
        val lateralDistance: Double = max(projectOntoMe(orthogonalDirection, vecBetweenPeds).norm,0.0)
        val f = (that.currentPosition - p.currentPosition).normalized * a0 * math.exp(-distanceBetweenPeds / r0) + orthogonalDirection * a1 * math.exp(-distanceBetweenPeds * lateralDistance / r1) +
          (p.currentPosition - that.currentPosition).normalized * smoothingPar3*math.exp(smoothingPar2*delta + smoothingPar1) + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocity).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocity).norm) * smoothingPar3*math.exp(smoothingPar2*delta + smoothingPar1)
        //println("in front:", sim.currentTime, p.ID, that.ID, distanceBetweenPeds, f)
        f
      } else {
        val f = (that.currentPosition - p.currentPosition).normalized * a0 * math.exp(-distanceBetweenPeds / r0) +
          (p.currentPosition - that.currentPosition).normalized * smoothingPar3*math.exp(smoothingPar2*delta + smoothingPar1) + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocity).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocity).norm) * smoothingPar3*math.exp(smoothingPar2*delta + smoothingPar1)
        //println("behind:",  sim.currentTime, p.ID, that.ID, distanceBetweenPeds, f)
        f
      }
  }

//100*exp(5.0*x+2.0)

  protected def computePedestrianIncrements(p: PedestrianSim): Unit = {


    // desired direction
    ///val currentDirection = computeDesiredDirection(p.currentPositionNew, p.currentDestinationNew)

    //def getPedInsideInfluenceArea: Iterable[PedestrianSim] = sim.findNeighbours(p.ID, 1.5)

    /** Attractive forces are composed of the will to reach the destination. */
      /*println(computePathFollowingComponent(p),
        getPedInsideInfluenceArea.foldLeft(new Vector2D(0.0, 0.0))((acc: NewBetterAcceleration2D, that: PedestrianSim) => acc + computePedestrianInteraction(p, that)),
        sim.spaceSF.walls.foldLeft(new Vector2D(0.0, 0.0))((acc: NewBetterAcceleration2D, w: Wall) => acc + obstacleInteractionAcceleration(p, w)))*/

    val totalAcceleration: Acceleration = {
      computePathFollowingComponent(p) +
        sim.findNeighbours(p.ID, 1.5).foldLeft(new Vector2D(0.0, 0.0))((acc: Acceleration, that: PedestrianSim) => acc + computePedestrianInteraction(p, that)) +
        sim.spaceSF.walls.foldLeft(new Vector2D(0.0, 0.0))((acc: Acceleration, w: Wall) => acc + obstacleInteractionAcceleration(p, w))
    }

    // if the pedestrian is not waiting at a gate, compute increments. Otherwise, return 0.0 for increments.
    if (!p.isWaiting) {
      // set travel time (could be done only at exit of system)
      p.travelTime = sim.currentTime - p.entryTime

      // sets the increments in position and velocity for the pedestrian
      p.velocityIncrement = totalAcceleration * sim.sf_dt.value
      p.positionIncrement = p.currentVelocity * sim.sf_dt.value

      //println(p.velocityIncrement , p.positionIncrement)
    }
  }

  def execute(): Unit = {

    // adds event to log
    sim.eventLogger.trace("time=" + sim.currentTime + ": moving pedestrians with NOMAD")

    // updates the position of the pedestrians
    sim.population.foreach(computePedestrianIncrements) // transformation of pedestrian objects
    sim.population.foreach(ped => {
      ped.move()
      ped.addHistory(sim.currentTime)
    }) // transformation of pedestrian objects


    sim.processCompletedPedestrian(sim.finalDestinationReached)
    // adds the pedestrians who reach the final destination to the completed list
    //sim.concatenate2PopulationCompleted(sim.population.filter(sim.finalDestinationReached))

    // removes the pedestrians which reached their final destination from the population and sets their "completed" flag to true
    //sim.removeFromPopulation(sim.finalDestinationReached)

    // enqueues pedestrians in the waiting zones if gating is used
    if (sim.useFlowGates) sim.population.foreach(sim.graph.enqueueInWaitingZone)

    // inserts next event
    insertNextEvent()
  }

  protected def insertNextEvent(): Unit = sim.insertEventWithDelayNew(sim.sf_dt)(new NOMADModel(sim))

}
