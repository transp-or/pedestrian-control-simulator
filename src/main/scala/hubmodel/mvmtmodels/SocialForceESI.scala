package hubmodel.mvmtmodels

import breeze.linalg.{DenseVector, max, min, norm}
import breeze.numerics.{cos, exp, sqrt}

import math.pow
import hubmodel._
import hubmodel.input.infrastructure.Wall

import scala.collection.mutable.ArrayBuffer


/**
  * Created by nicholas on 5/12/17.
  */
class SocialForceESI(sim: SFGraphSimulator) extends SocialForceLike(sim) with Action {

  /** Compute the force affecting the pedestrian. No need to pass pedestrian as argument as the function gets
    * the coordinate from the scope.
    *
    * @param pos point on the wall
    * @return force acting on the pedestrian from pos
    */
  protected def pedestrian2WallForce(ped: PedestrianSim, pos: Position): Force = {
    // set of parameters used for calculating the repulsive effects
    val A: Double = 10.0/0.2
    val B: Double = 0.2
    //val k1: Double = 1.2 * 100000.0
    //val k2: Double = 2.4 * 100000.0

    val dir: Direction = (ped.currentPosition - pos) / breeze.linalg.norm(pos - ped.currentPosition)
    val dirOrtho: Direction = DenseVector(-dir(1), dir(0))
    dir * (
      A * exp(-norm(ped.currentPosition - pos)/B))/* +
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
  protected def pedestrian2PedestrianForce(p1: PedestrianSim, p2: PedestrianSim): Force = {
    val A: Double = 1.52
    val B: Double = 0.21
    val lambda: Double =1.0// 0.75
    val dt: Double = 1.0
    //val f: Force = V / sigma * computeDiffEllipse(p1.currentPosition, p2.currentPosition, norm(p2.currentVelocity), computeDirection(p2.currentPosition, p2.currentDestination)) * exp(-computeEllipse(p1.currentPosition, p2.currentPosition, norm(p2.currentVelocity), computeDirection(p2.currentPosition, p2.currentDestination)) / sigma)
    //f * angleSightCoefficient(computeDirection(p1.currentPosition, p1.currentDestination), f)
    // angle of sight reduction

    val dab: Direction = p1.currentPosition-p2.currentPosition
    val yab: Direction = dt * p2.currentVelocity
    val bab: Double = 0.5*sqrt(pow(norm(dab) + norm(dab - yab),2) - pow(norm(yab),2))
    println(norm(dab), bab, exp((-bab)/B), exp((p1.r+p2.r-bab)/B), (norm(dab) + norm(dab-yab))/(4.0*bab), norm((dab/norm(dab) + (dab-yab)/norm(dab-yab))))

    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1.0-lambda)*0.5*(1.0+desiredDirection.dot(dab/norm(dab)))
    //println(dab, yab, bab, desiredDirection,desiredDirection.dot(dab/norm(dab)), exp(-bab/B), ((norm(dab) + norm(dab-yab))/2.0*bab) * 0.5 * (dab/norm(dab) + (dab-yab)/norm(dab-yab)))

    // final force
    //println(desiredDirection.dot(dab/norm(dab)), w)
    w*A*exp((p1.r+p2.r-bab)/B) * ((norm(dab) + norm(dab-yab))/(4.0*bab)) * (dab/norm(dab) + (dab-yab)/norm(dab-yab))

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

  protected def insertNextEvent(): Unit = sim.insertEventWithDelay(sim.sf_dt)(new SocialForceESI(sim))

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
