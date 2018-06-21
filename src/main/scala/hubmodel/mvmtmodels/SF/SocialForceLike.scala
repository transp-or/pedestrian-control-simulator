package hubmodel.mvmtmodels

import breeze.numerics.cos
import hubmodel.DES.SFGraphSimulator
import hubmodel._
import hubmodel.ped.PedestrianSim
import hubmodel.supply.continuous.Wall
import myscala.math.vector.ZeroVector2D

abstract class SocialForceLike(sim: SFGraphSimulator) {

  /** Computes the direction based on the current position and the target position
    *
    * @param pos  current position
    * @param goal target position
    * @return normalized direction
    */
  protected def computeDirection(pos: Position, goal: Position): Direction = {
    (goal - pos) / (goal - pos).norm
  }


  /** Based on the current velocity and the target velocity, computed the acceleration. The relaxation term is
    * hard coded into the function. This was done as the value I'v seen in all papers is a 1/0.5 seconds.
    * This can be changed by currying the function if needed.
    *
    * @param currentVel current velocity
    * @param desiredVel taregt velocity
    * @return acceleration
    */
  /*protected def computeAcceleration(currentVel: Velocity, desiredVel: Velocity): Force = {
    val relax: Double = 1.0 // relaxation term of 0.5s = multiplication by 2.0 s^-1 from original model
    //val relax: Double = 1.19 // parameters from other optimization (Social force model with explicit collision prediction)
    //val relax = 3.2
    //val relax = 0.84
    relax * (desiredVel - currentVel)
  }*/

  protected def computeAcceleration(currentVel: Velocity, desiredVel: Velocity): Acceleration = {
    val relax: Double = 1.0 // relaxation term of 0.5s = multiplication by 2.0 s^-1 from original model
    //val relax: Double = 1.19 // parameters from other optimization (Social force model with explicit collision prediction)
    //val relax = 3.2
    //val relax = 0.84
    (desiredVel - currentVel) * relax
  }

  /** Find the projection from of a point onto the wall
    *
    * @param pos point to project
    * @param w   [[Wall]] onto which the point must be projectec
    * @return The projected point
    */
  protected def computeProjection(pos: Position, w: Wall): Position = {
    val AP: Position = pos - w.startPoint //breeze.linalg.DenseVector(pos(0) - w.x1, pos(1) - w.y1)
    val AB: Position = w.endPoint - w.startPoint // breeze.linalg.DenseVector(w.x2 - w.x1, w.y2 - w.y1)
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

  /** Compute angle of sight reduction if direction between force and direction is larger
    * than a pre-specificed value.
    *
    * @param d direction of sight (walking direction)
    * @param f direction of force
    * @return coefficient
    */
  protected def angleSightCoefficient(d: Direction, f: Force): Double = {
    if ((d dot f) >= f.norm * cos(100 * math.Pi / 180)) 1.0
    else 0.5
  }

  /** Collect forces from population. The filter on the pedestrians does not take place here.
    * the argument 'pop' is already filtered.
    *
    * @param p        effected pedestrian
    * @param pop      population
    * @param forceAcc force accumulator, initially zero
    * @return sum of forces acting on pedestrian
    */
  protected def collectPedestrianRepulsionForces(p: PedestrianSim, pop: Iterable[PedestrianSim], forceAcc: Force): Force = {
    if (pop.isEmpty) forceAcc
    else if (pop.head == p) collectPedestrianRepulsionForces(p, pop.tail, forceAcc)
    else {
      collectPedestrianRepulsionForces(p, pop.tail, forceAcc + pedestrian2PedestrianForceNew(p, pop.head))
    }
  }

  /** Collects the wall repulsions for a specific pedestrian.
    *
    * @param ped      pedestrian
    * @param walls    collection of all walls
    * @param forceAcc accumulator
    * @return the some of all the effects from the walls
    */
  protected def collectWallRepulsionForces(ped: PedestrianSim, walls: Iterable[Wall], forceAcc: Force): Force = {

    // Collection of all the forces.
    // TODO: not collect all forces but only ones within distance and deal with doorways
    if (walls.isEmpty) forceAcc
    else {
      val closestPoint = getClosestPoint(ped.currentPosition, walls.head)
      collectWallRepulsionForces(ped, walls.tail, forceAcc + pedestrian2WallForceNew(ped, closestPoint))
    }
  }

  /** compute increments of position and velocity for a specific pedestrian
    *
    * @param p pedestrian
    * @return increments in Position and Velocity
    */
  protected def computePedestrianIncrements(p: PedestrianSim): Unit = {


    // desired direction
    val currentDirection = computeDirection(p.currentPosition, p.currentDestination)

    /** Attractive forces are composed of the will to reach the destination. */
    val collectAttractiveForce: Force = {
      //if (p.currentVelocity.exists(_.isNaN)) { enterDebugMethod }
      //assert(!computeAcceleration(p.currentVelocity, currentDirection * p.freeFlowVel).forall(_.isNaN), "acceleration is NaN: " + p.currentVelocity.toString() + ", " + currentDirection * p.freeFlowVel)
      //println(p.ID, p.freeFlowVel, p.currentVelocity, currentDirection)
      computeAcceleration(p.currentVelocity, currentDirection * p.freeFlowVel)
    }

    /** Repulsive forces created by the walls and the other pedestrians. Requires access to the full population
      * stored in the simulation. */
    val collectRepulsiveForces: Force = {
      //if (collectPedestrianRepulsionForces(p, sim.population.filterNot(ped => norm(p.currentPosition - ped.currentPosition) > 15.0), breeze.linalg.DenseVector(0.0, 0.0)).exists(_.isNaN)) { enterDebugMethod }

      collectWallRepulsionForces(p, sim.walls, new ZeroVector2D) +
        collectPedestrianRepulsionForces(p, sim.findNeighbours(p.ID, 5.0), new ZeroVector2D)
    }


    // if the pedestrian is not waiting at a gate, compute increments. Otherwise, return 0.0 for increments.
    if (!p.isWaiting) {
      // set travel time (could be done only at exit of system)
      p.travelTime = sim.currentTime - p.entryTime

      // collect forces
      val force: Force = collectAttractiveForce + collectRepulsiveForces
      //println(p.ID, collectAttractiveForce, collectRepulsiveForces, force)


      // sets the increments in position and velocity for the pedestrian
      p.velocityIncrement = force * sim.sf_dt.value
      p.positionIncrement = (p.currentVelocity + p.velocityIncrement) * sim.sf_dt.value // (p.currentVelocity + p.velocityIncrement)

      //println(p.velocityIncrementNew , p.positionIncrementNew)
    }
  }

  /** Method called by the main simulator to makes things happen. In this case makes the pedestrians move.
    *
    */
  def execute(): Unit = {

    // adds event to log
    sim.eventLogger.trace("time=" + sim.currentTime + ": moving pedestrians")

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
    //sim.rebuildMTree()

    // enqueues pedestrians in the waiting zones if gating is used
    if (sim.useFlowGates) sim.population.foreach(sim.graph.enqueueInWaitingZone)


    // inserts next event
    insertNextEvent()
  }

  protected def insertNextEvent(): Unit

  protected def pedestrian2PedestrianForce(p1: PedestrianSim, p2: PedestrianSim): Force = new ZeroVector2D

  protected def pedestrian2WallForce(ped: PedestrianSim, pos: Position): Force = new ZeroVector2D

  protected def pedestrian2PedestrianForceNew(p1: PedestrianSim, p2: PedestrianSim): Force = new ZeroVector2D

  protected def pedestrian2WallForceNew(ped: PedestrianSim, pos: Position): Force = new ZeroVector2D

}