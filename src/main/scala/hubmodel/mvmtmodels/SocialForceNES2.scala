package hubmodel.mvmtmodels

import breeze.linalg.{DenseVector, norm}
import breeze.numerics.{exp, sqrt}
import hubmodel._

import scala.math.pow


/**
  * Created by nicholas on 5/12/17.
  */
class SocialForceNES2(sim: SFGraphSimulator) extends SocialForceLike(sim) with Action {

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
    val dir: Direction = (ped.currentPosition - pos)/breeze.linalg.norm(pos-ped.currentPosition)
    val dirOrtho: Direction = DenseVector(-dir(1), dir(0))
    //dir*( A*exp((ped.r - breeze.linalg.norm(ped.currentPosition-pos))/B) )
    DenseVector(0.0,0.0)
  }

  protected def pedestrian2PedestrianForce(p1: PedestrianSim, p2: PedestrianSim): Force = {

    // parameters from original article (On modeling and evolutionary optimization of nonlinearly coupled pedestrian interactions)
    val A: Double = 3.6
    val B: Double = 0.53
    val lambda: Double = 0.67
    val dt: Double = 2.0 // [s]
    //val tau: Double = sim.dt // [s]

    // parameters from other optimization (Social force model with explicit collision prediction)
    //val A: Double = 1.33 // [m s^-2]
    //val B: Double = 0.34 // [m]
    //val lambda: Double = 0.08 // []
    //val tau: Double = 1.78 // [s]

    // vector from pedestrian2 pointing to pedestrian 1
    val dab: Direction = p1.currentPosition-p2.currentPosition
    val yab: Direction = dab - dt * (p2.currentVelocity - p1.currentVelocity)
    val bab: Double = 0.5*sqrt((pow(norm(dab) + norm(dab - yab),2) - pow(norm(yab),2))/(1+dt*norm(p1.currentVelocity)))

    // angle of sight reduction
    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1.0-lambda)*0.5*(1.0+desiredDirection.dot(dab/norm(dab)))
    //println(dab, yab, bab, desiredDirection,desiredDirection.dot(dab/norm(dab)),  w, exp(-bab/B), ((norm(dab) + norm(dab-yab))/2.0*bab) * 0.5 * (dab/norm(dab) + (dab-yab)/norm(dab-yab)))

    // final force
    w*A*exp(-bab/B)*dab/norm(dab)

    // used by the interpretation of the model in (Social force model with explicit collision prediction)
    //val y12: Position = d21 - (p2.currentVelocity-p1.currentVelocity)*tau
    //if (norm(d21 - y12) > 0.0) w*(A*exp(-b/B))/sqrt(denominator) * (distance + norm(d21 - y12)) / (4*b) * (d21/distance + (d21 - y12)/norm(d21 - y12))
    //else w*(A*exp(-b/B))/sqrt(denominator) * distance / (4*b) * (d21/distance)
  }


  protected def insertNextEvent(): Unit = sim.insertEventWithDelay(sim.sf_dt) (new SocialForceNES2(sim))
}

