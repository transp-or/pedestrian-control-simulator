package hubmodel.mvmtmodels.NOMAD

import breeze.linalg.max
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel._
import hubmodel.ped.PedestrianSim
import hubmodel.supply.continuous.Wall
import hubmodel.tools.cells.isInVertex
import myscala.math.vector.{Vector2D, ZeroVector2D}


class NOMADOriginalModelV2(sim: SFGraphSimulator) extends Action {


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
    if ((w.startPoint - point).norm < (w.endPoint - point).norm) {
      w.startPoint
    }
    else {
      w.endPoint
    }
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
    val shy: Double = 0.35 // whats is the value of this parameter ?
    val aW: Double = -1.0 * 5.0 // multiplication by -1 to take into account the negative sign
    val p2wall: Vector2D = getClosestPoint(p.currentPosition, w) - p.currentPosition
    val d: Double = p2wall.norm
    if (p.r - d > 0.0) {
      val k0: Double = -p.k0
      //-1000.0
      val k1: Double = -p.k1 //1000.0
      //p2wall.normalized * max(math.pow(10,-5), p.r - d) * k0 + p2wall.orthogonal.dot(p.currentVelocityNew) * max(math.pow(10,-5), p.r - d) * k1
      p2wall.normalized * (p.r - d) * k0
      p2wall.orthogonal * (-p.k1 * (p.r - d) * p.currentVelocity.dot(p2wall.orthogonal)) - p2wall.normalized * p.k0
    } else {
      val factor: Double = math.min(1.0, 1.0 - (d - p.r - shy) / shy)
      if (factor > 0.0) {
        p2wall.normalized * factor * aW
      }
      else new ZeroVector2D
    }

    /*if (d > shy) new ZeroVector2D
    else if (d <= 0.5 * shy) p2wall.normalize * aW
    else p2wall.normalize * (1.0 - (d-shy) / shy) * aW*/
  }

  private def projectOntoMe(me: Vector2D, that: Vector2D): Vector2D = {
    me * ((me dot that) / (me.norm * me.norm))
  }

  private def computePedestrianInteraction(p: PedestrianSim, that: PedestrianSim): Acceleration = {
    val a0: Double = -1.0 * p.a0
    //10.0//2.02 // multiplication by -1 to take into account the negative sign
    val r0: Double = p.r0
    //0.16//1.24
    val a1: Double = -1.0 * p.a1
    //10.0//7.08 // multiplication by -1 to take into account the negative sign
    val r1: Double = p.r1
    //0.16//0.69
    val anticipationTime: Double = 0.5

    val (pPos, thatPos) = if (p.currentVelocity.dot(that.currentVelocity) > 0.0) { // don't need to normalize as sign is of interest
      (p.currentPosition, that.currentPosition)
    } else {
      (p.currentPosition + p.currentVelocity * anticipationTime, that.currentPosition + that.currentVelocity * anticipationTime)
    }
    val vecBetweenPeds: Vector2D = thatPos - pPos
    val distanceBetweenPeds: Double = max(vecBetweenPeds.norm, math.pow(10, -5))

    //if (p.currentVelocityNew.norm > 0.0 && ((p.currentPositionNew - that.currentPositionNew).norm <= (p.r + that.r) )) {
    val delta: Double = math.max((p.r + that.r) - (p.currentPosition - that.currentPosition).norm, math.pow(10, -5))
    val k0: Double = -p.k0
    //-1000.0
    val k1: Double = p.k1 //1000.0

    val tmpOrthogonalDirectionCloseRange: Direction = new Direction((that.currentPosition - p.currentPosition).Y, (that.currentPosition - p.currentPosition).X * -1.0).normalized
    val orthogonalDirectionCloseRange: Direction = if (tmpOrthogonalDirectionCloseRange.dot(p.currentVelocity) < 0.0) {
      tmpOrthogonalDirectionCloseRange * -1.0
    } else {
      tmpOrthogonalDirectionCloseRange
    }

    //(that.currentPositionNew - p.currentPositionNew).normalize * k0 * delta + orthogonalDirection*(that.currentVelocityNew.norm - p.currentVelocityNew.norm) * k1 * delta
    if (p.currentVelocity.norm > 0.0 && p.currentVelocity.dot(vecBetweenPeds) < 0.0) {
      val orthogonalDirection: Direction = projectOntoMe(new Direction(p.currentVelocity.Y, p.currentVelocity.X * -1.0).normalized, vecBetweenPeds).normalized
      val lateralDistance: Double = max(projectOntoMe(orthogonalDirection, vecBetweenPeds).norm, math.pow(10, -5))
      val f = (that.currentPosition - p.currentPosition).normalized * a0 * math.exp(-distanceBetweenPeds / r0) + orthogonalDirection * a1 * math.exp(-distanceBetweenPeds * lateralDistance / r1) +
        (that.currentPosition - p.currentPosition).normalized * delta * k0 + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocity).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocity).norm) * delta * k1
      //println("in front:", sim.currentTime, p.ID, that.ID, distanceBetweenPeds, f)
      f
    } else {
      val f = (that.currentPosition - p.currentPosition).normalized * a0 * math.exp(-distanceBetweenPeds / r0) +
        (that.currentPosition - p.currentPosition).normalized * delta * k0 + (projectOntoMe(orthogonalDirectionCloseRange, that.currentVelocity).norm - projectOntoMe(orthogonalDirectionCloseRange, p.currentVelocity).norm) * delta * k1
      //println("behind:",  sim.currentTime, p.ID, that.ID, distanceBetweenPeds, f)
      f
    }
  }

  /*def pedestrianPhysical(kappa: Double, thisSpeed: Velocity, k0: Double, pedDistanceData: Nothing): Vector2D = {
    val gpq = Math.max(pedDistanceData.gpq, 1.0E-5D)
    val tpq = new Vector2D(-pedDistanceData.dx.y, pedDistanceData.dx.x)
    //tpq.set(-pedDistanceData.dx.y, pedDistanceData.dx.x, pedDistanceData.dx.z)
    val dV = new Velocity(pedDistanceData.pedestrianSpeed.x, pedDistanceData.pedestrianSpeed.y)
    //dV.sub(thisSpeed)
    val tpq2 = (dV-thisSpeed).dot(tpq) * kappa  * gpq
    val pedDistance = Vector2D(pedDistanceData.dx.x * (-k0 * gpq), pedDistanceData.dx.y * (-k0 * gpq))
    pedDistance + tpq2
  }*/

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
        sim.walls.foldLeft(new Vector2D(0.0, 0.0))((acc: Acceleration, w: Wall) => acc + obstacleInteractionAcceleration(p, w))
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

    /*if (p.getHistoryPosition.size >= 11 && p.getHistoryPosition.takeRight(10).zip(p.getHistoryPosition.dropRight(1).takeRight(10)).forall(poss => (poss._2._2 - poss._1._2).norm < 0.01) ) {
      println(p.ID, p.getHistoryPosition.last, p.getHistoryPosition.dropRight(1).last, p.currentDestination, sim.currentTime)
    }*/


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
      sim.controlDevices.flowGates.foreach(fg => {
        sim.population
          .filter(p => p.nextZone == fg.endVertex && !fg.pedestrianQueue.contains(p) && !p.freedFrom.contains(fg.ID) && isInVertex(fg.startVertex)(p.currentPosition))
          .foreach(p => sim.insertEventWithZeroDelay(new fg.EnqueuePedestrian(p, sim)))
      })
    }


    // inserts next event
    insertNextEvent()
  }

  protected def insertNextEvent(): Unit = sim.insertEventWithDelayNew(sim.sf_dt)(new NOMADOriginalModelV2(sim))

}
