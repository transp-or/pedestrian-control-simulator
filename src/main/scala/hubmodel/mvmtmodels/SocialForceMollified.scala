package hubmodel.mvmtmodels

import breeze.numerics.{exp, sqrt}
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel._
import hubmodel.ped.PedestrianSim

import scala.math.pow


/**
  * Created by nicholas on 5/12/17.
  */
class SocialForceMollified(sim: SFGraphSimulator) extends SocialForceLike(sim) with Action {

  // set epsilon to 0.1
  override protected def computeDirection(pos: Position, goal: Position): Direction = {
    (goal - pos) / sqrt(pow(goal.X - pos.X, 2) + pow(goal.Y - pos.Y, 2) + 0.1 * 0.1)
  }

  /** Compute the force affecting the pedestrian. No need to pass pedestrian as argument as the function gets
    * the coordinate from the scope.
    *
    * @param pos point on the wall
    * @return force acting on the pedestrian from pos
    */
  protected override def pedestrian2WallForce(ped: PedestrianSim, pos: Position): Force = {
    // set of parameters used for calculating the repulsive effects
    val A: Double = 20.1
    val B: Double = 0.25
    //val k1: Double = 1.2 * 100000.0
    //val k2: Double = 2.4 * 100000.0

    val dir: Direction = (ped.currentPosition - pos) / (pos - ped.currentPosition).norm
    val dirOrtho: Direction = dir.orthogonal
    dir * (
      A * exp((ped.currentPosition - pos).norm * -1 / B)) /* +
        k1 * max(0.0, ped.r - breeze.linalg.norm(ped.currentPosition - pos))
      ) +
      k2 * max(0.0, ped.r - breeze.linalg.norm(ped.currentPosition - pos)) * ped.currentVelocity.dot(dirOrtho) * dirOrtho*/
  }

  /** Classical social force interactions between pedestrians.
    *
    * @param p1 pedestrian to calculate forces on
    * @param p2 pedestrian creating force
    * @return force acting on p1 created by p2
    */
  protected override def pedestrian2PedestrianForce(p1: PedestrianSim, p2: PedestrianSim): Force = {
    val A: Double = 2.1 / 0.3
    val B: Double = 0.3
    val lambda: Double = 1.0
    val tau: Double = 1.0
    val dt: Double = 0.5
    //val f: Force = V / sigma * computeDiffEllipse(p1.currentPosition, p2.currentPosition, norm(p2.currentVelocity), computeDirection(p2.currentPosition, p2.currentDestination)) * exp(-computeEllipse(p1.currentPosition, p2.currentPosition, norm(p2.currentVelocity), computeDirection(p2.currentPosition, p2.currentDestination)) / sigma)
    //f * angleSightCoefficient(computeDirection(p1.currentPosition, p1.currentDestination), f)
    // angle of sight reduction

    val dab: Direction = p1.currentPosition - p2.currentPosition
    val yab: Direction = p2.currentVelocity*dt
    val bab: Double = 0.5 * sqrt(pow(dab.norm + (dab - yab).norm, 2) - pow((yab).norm, 2))

    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1.0 - lambda) * 0.5 * (1.0 + desiredDirection.dot(dab / (dab.norm)))
    //println(dab, yab, bab, desiredDirection, desiredDirection.dot(dab / (dab.norm)), exp(-bab / B), ((norm(dab) + (dab - yab).norm) / 2.0 * bab) * 0.5 * (dab / dab.norm + (dab - yab) / (dab - yab).norm))

    // final force
    (dab / dab.norm + (dab - yab) / (dab - yab).norm)  * A * exp(-bab / B) * ((dab.norm + (dab - yab).norm) / 2.0 * bab) * 0.5

    /*
    val d21: Direction = p1.currentPosition-p2.currentPosition
    val distance: Double = breeze.linalg.norm(d21)
    val b = computeEllipse(p1.currentPosition, p2.currentPosition, norm(p2.currentVelocity), computeDirection(p2.currentPosition, p2.currentDestination))
    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1-lambda)*0.5*(1.0+cos(desiredDirection.dot(d21) / distance))
    // used by the interpretation of the model in (Social force model with explicit collision prediction)
    val y12: Position = d21 - p2.currentVelocity*tau
    if (norm(d21 - y12) > 0.0) w*(A*exp(-b/B)) * (distance + norm(d21 - y12)) / (4*b) * (d21/distance + (d21 - y12)/norm(d21 - y12))
    else w*A*exp(-b/B) * distance / (4*b) * (d21/distance)*/
  }

  protected def insertNextEvent(): Unit = sim.insertEventWithDelay(sim.sf_dt)(new SocialForceMollified(sim))

  /*
  private def computeDiffEllipse(pos1: Position, pos2: Position, vel2: Double, dir2: Direction): Force = {
    val d21: Position = pos1 - pos2
    val distance: Double = breeze.linalg.norm(d21)
    0.5 * (distance + norm(d21 - vel2*sim.dt * dir2)) * (d21 / distance + d21 / norm(d21 - vel2*sim.dt * dir2)) / sqrt(pow(distance + norm(d21 - vel2*sim.dt * dir2), 2) - pow(vel2*sim.dt, 2))
  }

  private def computeEllipse(pos1: Position, pos2: Position, vel2: Double, dir2: Direction): Double = {
    val d21: Position = pos1 - pos2 // vector from ped 2 to ped 1
    0.5*sqrt(pow(breeze.linalg.norm(d21) + breeze.linalg.norm(d21 - vel2*sim.dt*dir2), 2) - pow(vel2*sim.dt, 2))
  }*/
}
