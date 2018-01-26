package hubmodel.mvmtmodels

import breeze.linalg.max
import hubmodel.input.infrastructure.Wall
import hubmodel.{Action, NewBetterAcceleration2D, NewBetterDirection2D, NewBetterForce2D, NewBetterPosition2D, NewBetterVelocity2D, NewTime, PedestrianSim, SFGraphSimulator, Vector2D, ZeroVector2D, distance, normNew}

class NOMADModel(sim: SFGraphSimulator) extends Action {


  /** Computes the direction based on the current position and the target position
    *
    * @param pos  current position
    * @param goal target position
    * @return normalized direction
    */
  protected def computeDesiredDirection(pos: NewBetterPosition2D, goal: NewBetterPosition2D): NewBetterDirection2D = {
    (goal - pos) / normNew(goal - pos)
  }

  protected def computePathFollowingComponent(p: PedestrianSim): NewBetterAcceleration2D = {
    val tau: Double = 0.62
    (computeDesiredDirection(p.currentPositionNew, p.currentDestinationNew) * p.freeFlowVel - p.currentVelocityNew) / tau
  }


  /** Find the projection from of a point onto the wall
    *
    * @param pos point to project
    * @param w   [[Wall]] onto which the point must be projectec
    * @return The projected point
    */
  protected def computeProjection(pos: NewBetterPosition2D, w: Wall): NewBetterPosition2D = {
    val AP: NewBetterPosition2D = pos - new NewBetterPosition2D(w.x1, w.y1)
    val AB: NewBetterPosition2D = new NewBetterPosition2D(w.x2, w.y2) - new NewBetterPosition2D(w.x1, w.y1)
    AB * ((AB dot AP) / (AB dot AB)) + new NewBetterPosition2D(w.x1, w.y1)
  }

  /** True of the point is on the wall, false if not
    *
    * @param point point to check
    * @param w     wall
    * @return boolean whether indictating if the point is on the wall
    */
  protected def isOnWall(point: NewBetterPosition2D, w: Wall): Boolean = {
    ((w.x1 <= point.X && point.X <= w.x2) && (w.y1 <= point.Y && point.Y <= w.y2)) || ((w.x2 <= point.X && point.X <= w.x1) && (w.y2 <= point.Y && point.Y <= w.y1))
  }

  /** Finds the closest end point of a wall if the point is not on the wall
    *
    * @param point point which is not on the wall
    * @param w     wall to finds end from
    * @return closest end point of the wall to the point
    */
  protected def getClosestEndPoint(point: NewBetterPosition2D, w: Wall): NewBetterPosition2D = {
    if (normNew(new NewBetterPosition2D(w.startPoint(0), w.startPoint(1)) - point) < normNew(new NewBetterPosition2D(w.endPoint(0), w.endPoint(1)) - point)) new NewBetterPosition2D(w.startPoint(0), w.startPoint(1))
    else new NewBetterPosition2D(w.endPoint(0), w.endPoint(1))
  }

  /** Find the point used to compute the repulsive effects from the wall onto a pedestrian.
    *
    * @param pos Position of the pedestrian
    * @param w   wall to calculate repulsion from
    * @return position used to calculate repulsion
    */
  protected def getClosestPoint(pos: NewBetterPosition2D, w: Wall): NewBetterPosition2D = {
    val proj: NewBetterPosition2D = computeProjection(pos, w)
    if (!isOnWall(proj, w)) getClosestEndPoint(pos, w)
    else proj
  }

  protected def obstacleInteractionAcceleration(p: PedestrianSim, w: Wall): NewBetterAcceleration2D = {
    val shy: Double = 1.0 // whats is the value of this parameter ?
    val aW: Double = -1.0 * 10.0 // multiplication by -1 to take into account the negative sign
    val p2wall: Vector2D = getClosestPoint(p.currentPositionNew, w) - p.currentPositionNew
    val d: Double = p2wall.norm
    if (d > shy) new ZeroVector2D
    else if (d <= 0.5 * shy) p2wall.normalize * aW
    else p2wall.normalize * 2.0 * (1.0 - d / shy) * aW
  }

  private def projectOntoMe(me: Vector2D, that: Vector2D): Vector2D = {
    me * ((me dot that) / (me.norm * me.norm))
  }

  private def computePedestrianInteraction(p: PedestrianSim, that: PedestrianSim): NewBetterAcceleration2D = {
    val a0: Double = -1.0 * 2.02 // multiplication by -1 to take into account the negative sign
    val r0: Double = 1.24
    val a1: Double = -1.0 * 7.08 // multiplication by -1 to take into account the negative sign
    val r1: Double = 0.69
    val anticipationTime: Double = 0.53

    val (pPos, thatPos) = if (p.currentVelocityNew.dot(that.currentVelocityNew) > 0.0) { // don't need to normalize as sign is of interest
      (p.currentPositionNew, that.currentPositionNew)
    } else {
      (p.currentPositionNew + p.currentVelocityNew * anticipationTime, that.currentPositionNew + that.currentVelocityNew * anticipationTime)
    }
    val vecBetweenPeds: Vector2D = thatPos - pPos
    val distanceBetweenPeds: Double = max(vecBetweenPeds.norm, 0.0*(p.r + that.r))

    //if (p.currentVelocityNew.norm > 0.0 && ((p.currentPositionNew - that.currentPositionNew).norm <= (p.r + that.r) )) {
      val delta: Double = max((p.r + that.r) - (p.currentPositionNew - that.currentPositionNew).norm, 0.0)
      val k0: Double = -10.0
      val k1: Double = 10.0
      val orthogonalDirection: NewBetterDirection2D = projectOntoMe(new NewBetterDirection2D(p.currentVelocityNew.Y, p.currentVelocityNew.X * -1.0).normalize, vecBetweenPeds)
    val orthogonalDirectionCloseRange: NewBetterDirection2D = new NewBetterDirection2D((that.currentPositionNew - p.currentPositionNew).Y, (that.currentPositionNew - p.currentPositionNew).X * -1.0).normalize

    //(that.currentPositionNew - p.currentPositionNew).normalize * k0 * delta + orthogonalDirection*(that.currentVelocityNew.norm - p.currentVelocityNew.norm) * k1 * delta
      if (p.currentVelocityNew.norm > 0.0 && p.currentVelocityNew.dot(thatPos) > 0.0) {
        val lateralDistance: Double = max(projectOntoMe(orthogonalDirection, vecBetweenPeds).norm,0.0)
        (that.currentPositionNew - p.currentPositionNew).normalize * a0 * math.exp(-distanceBetweenPeds / r0) + orthogonalDirection * a1 * math.exp(-distanceBetweenPeds * lateralDistance / r1) +
          (that.currentPositionNew - p.currentPositionNew).normalize * k0 * delta + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocityNew) - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocityNew).norm) * k1 * delta

      } else {
        (that.currentPositionNew - p.currentPositionNew).normalize * a0 * math.exp(-distanceBetweenPeds / r0) +
          (that.currentPositionNew - p.currentPositionNew).normalize * k0 * delta + orthogonalDirectionCloseRange*(projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocityNew).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocityNew).norm) * k1 * delta
      }
  }


  protected def computePedestrianIncrements(p: PedestrianSim): Unit = {


    // desired direction
    val currentDirection = computeDesiredDirection(p.currentPositionNew, p.currentDestinationNew)

    def getPedInsideInfluenceArea: Iterable[PedestrianSim] = sim.findNeighbours(p.ID, 1.0)

    /** Attractive forces are composed of the will to reach the destination. */
    val totalAcceleration: NewBetterAcceleration2D = {
      computePathFollowingComponent(p) +
        getPedInsideInfluenceArea.foldLeft(new Vector2D(0.0, 0.0))((acc: NewBetterAcceleration2D, that: PedestrianSim) => acc + computePedestrianInteraction(p, that)) +
        sim.spaceSF.walls.foldLeft(new Vector2D(0.0, 0.0))((acc: NewBetterAcceleration2D, w: Wall) => acc + obstacleInteractionAcceleration(p, w))
    }


    // if the pedestrian is not waiting at a gate, compute increments. Otherwise, return 0.0 for increments.
    if (!p.isWaiting) {
      // set travel time (could be done only at exit of system)
      p.travelTime = sim.currentTime - p.entryTime

      // sets the increments in position and velocity for the pedestrian
      p.velocityIncrementNew = totalAcceleration * sim.sf_dt
      p.positionIncrementNew = p.currentVelocityNew * sim.sf_dt.toDouble

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
    // adds the pedestrians who reach the final destination to the completed list
    sim.concatenate2PopulationCompleted(sim.population.filter(sim.finalDestinationReached))

    // removes the pedestrians whic reached their final destination from the population
    sim.removeFromPopulation(sim.finalDestinationReached)
    sim.rebuildMTree()

    // enqueues pedestrians in the waiting zones if gating is used
    if (sim.useFlowGates) sim.population.foreach(sim.graph.enqueueInWaitingZone)

    // inserts next event
    insertNextEvent()
  }

  protected def insertNextEvent(): Unit = sim.insertEventWithDelayNew(new NewTime(sim.sf_dt))(new NOMADModel(sim))

}
