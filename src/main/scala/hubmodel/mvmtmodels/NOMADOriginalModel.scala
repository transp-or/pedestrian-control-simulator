package hubmodel.mvmtmodels

import breeze.linalg.max
import hubmodel.supply.Wall
import hubmodel._

class NOMADOriginalModel(sim: SFGraphSimulator) extends Action {


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
    val shy: Double = 0.35 // whats is the value of this parameter ?
    val aW: Double = -1.0 * 5.0 // multiplication by -1 to take into account the negative sign
    val p2wall: Vector2D = getClosestPoint(p.currentPositionNew, w) - p.currentPositionNew
    val d: Double = p2wall.norm
    if (p.r - d > 0.0){
      val k0: Double = -p.k0//-1000.0
      val k1: Double = -p.k1//1000.0
      //p2wall.normalized * max(math.pow(10,-5), p.r - d) * k0 + p2wall.orthogonal.dot(p.currentVelocityNew) * max(math.pow(10,-5), p.r - d) * k1
      p2wall.normalized * (p.r - d) * k0
      p2wall.orthogonal * (-p.k1 * (p.r - d) * p.currentVelocityNew.dot(p2wall.orthogonal)) - p2wall.normalized * p.k0
    } else {
      val factor: Double = math.min(1.0, 1.0 - (d - p.r - shy) / shy)
      if (factor > 0.0) {p2wall.normalized * factor * aW}
      else new ZeroVector2D
    }

    /*if (d > shy) new ZeroVector2D
    else if (d <= 0.5 * shy) p2wall.normalize * aW
    else p2wall.normalize * (1.0 - (d-shy) / shy) * aW*/
  }

  private def projectOntoMe(me: Vector2D, that: Vector2D): Vector2D = {
    me * ((me dot that) / (me.norm * me.norm))
  }

  private def computePedestrianInteraction(p: PedestrianSim, that: PedestrianSim): NewBetterAcceleration2D = {
    val a0: Double = -1.0 * p.a0//10.0//2.02 // multiplication by -1 to take into account the negative sign
    val r0: Double = p.r0//0.16//1.24
    val a1: Double = -1.0 * p.a1//10.0//7.08 // multiplication by -1 to take into account the negative sign
    val r1: Double = p.r1//0.16//0.69
    val anticipationTime: Double = 0.5

    val (pPos, thatPos) = if (p.currentVelocityNew.dot(that.currentVelocityNew) > 0.0) { // don't need to normalize as sign is of interest
      (p.currentPositionNew, that.currentPositionNew)
    } else {
      (p.currentPositionNew + p.currentVelocityNew * anticipationTime, that.currentPositionNew + that.currentVelocityNew * anticipationTime)
    }
    val vecBetweenPeds: Vector2D = thatPos - pPos
    val distanceBetweenPeds: Double = max(vecBetweenPeds.norm, math.pow(10,-5))

    //if (p.currentVelocityNew.norm > 0.0 && ((p.currentPositionNew - that.currentPositionNew).norm <= (p.r + that.r) )) {
    val delta: Double = math.max((p.r + that.r) - (p.currentPositionNew - that.currentPositionNew).norm, math.pow(10,-5))
    val k0: Double = -p.k0//-1000.0
    val k1: Double = p.k1//1000.0

    val tmpOrthogonalDirectionCloseRange: NewBetterDirection2D = new NewBetterDirection2D((that.currentPositionNew - p.currentPositionNew).Y, (that.currentPositionNew - p.currentPositionNew).X * -1.0).normalized
    val orthogonalDirectionCloseRange: NewBetterDirection2D = if (tmpOrthogonalDirectionCloseRange.dot(p.currentVelocityNew) < 0.0) {tmpOrthogonalDirectionCloseRange * -1.0} else {tmpOrthogonalDirectionCloseRange}

    //(that.currentPositionNew - p.currentPositionNew).normalize * k0 * delta + orthogonalDirection*(that.currentVelocityNew.norm - p.currentVelocityNew.norm) * k1 * delta
      if (p.currentVelocityNew.norm > 0.0 && p.currentVelocityNew.dot(vecBetweenPeds) < 0.0) {
        val orthogonalDirection: NewBetterDirection2D = projectOntoMe(new NewBetterDirection2D(p.currentVelocityNew.Y, p.currentVelocityNew.X * -1.0).normalized, vecBetweenPeds).normalized
        val lateralDistance: Double = max(projectOntoMe(orthogonalDirection, vecBetweenPeds).norm, math.pow(10,-5))
        val f = (that.currentPositionNew - p.currentPositionNew).normalized * a0 * math.exp(-distanceBetweenPeds / r0) + orthogonalDirection * a1 * math.exp(-distanceBetweenPeds * lateralDistance / r1) +
          (that.currentPositionNew - p.currentPositionNew).normalized * delta*k0 + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocityNew).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocityNew).norm) * delta*k1
        //println("in front:", sim.currentTime, p.ID, that.ID, distanceBetweenPeds, f)
        f
      } else {
        val f = (that.currentPositionNew - p.currentPositionNew).normalized * a0 * math.exp(-distanceBetweenPeds / r0) +
          (that.currentPositionNew - p.currentPositionNew).normalized * delta*k0 + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocityNew).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocityNew).norm) * delta*k1
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

    val totalAcceleration: NewBetterAcceleration2D = {
      computePathFollowingComponent(p) +
        sim.findNeighbours(p.ID, 1.5).foldLeft(new Vector2D(0.0, 0.0))((acc: NewBetterAcceleration2D, that: PedestrianSim) => acc + computePedestrianInteraction(p, that)) +
        sim.spaceSF.walls.foldLeft(new Vector2D(0.0, 0.0))((acc: NewBetterAcceleration2D, w: Wall) => acc + obstacleInteractionAcceleration(p, w))
    }

    // if the pedestrian is not waiting at a gate, compute increments. Otherwise, return 0.0 for increments.
    if (!p.isWaiting) {
      // set travel time (could be done only at exit of system)
      p.travelTime = sim.currentTime - p.entryTime

      // sets the increments in position and velocity for the pedestrian
      p.velocityIncrementNew = totalAcceleration * sim.sf_dt.value
      p.positionIncrementNew = p.currentVelocityNew * sim.sf_dt.value

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
    sim.processCompletedPedestrian(sim.finalDestinationReached)

    //sim.concatenate2PopulationCompleted(sim.population.filter(sim.finalDestinationReached))

    // removes the pedestrians whic reached their final destination from the population
    //sim.removeFromPopulation(sim.finalDestinationReached)
    sim.rebuildMTree()

    // enqueues pedestrians in the waiting zones if gating is used
    if (sim.useFlowGates) {
      sim.graph.flowGates.foreach(fg => {
        sim.population
          .filter(p => !fg.pedestrianQueue.contains(p) && !p.freedFrom.contains(fg.ID) && isInVertex(fg.startVertex)(p.currentPositionNew) )
          .foreach(p => sim.insertEventWithZeroDelay(new fg.EnqueuePedestrian(p, sim)))
      })
      //sim.population.foreach(sim.graph.enqueueInWaitingZone)
    }


    // inserts next event
    insertNextEvent()
  }

  protected def insertNextEvent(): Unit = sim.insertEventWithDelayNew(sim.sf_dt)(new NOMADOriginalModel(sim))

}
