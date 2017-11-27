package hubmodel.mvmtmodels

import breeze.linalg.{DenseVector, norm}
import breeze.numerics.{cos, exp, sqrt}
import hubmodel._

import scala.math.pow


/**
  * Created by nicholas on 5/12/17.
  */
class SocialForceESII(sim: SFGraphSimulator) extends SocialForceLike(sim) with Action {

  /** Compute the force affecting the pedestrian. No need to pass pedestrian as argument as the function gets
    * the coordinate from the scope.
    *
    * @param pos point on the wall
    * @return force acting on the pedestrian from pos
    */
  protected def pedestrian2WallForce(ped: PedestrianSim, pos: Position): Force = {
    // set of parameters used for calculating the repulsive effects
    val A: Double = 5.5
    val B: Double = 0.5

    val dir: Direction = (ped.currentPosition - pos)/breeze.linalg.norm(pos-ped.currentPosition)
    val dirOrtho: Direction = DenseVector(-dir(1), dir(0))
    dir*( A*exp((-norm(ped.currentPosition-pos))/B) )
    //DenseVector(0.0,0.0)
  }

  protected def pedestrian2PedestrianForce(p1: PedestrianSim, p2: PedestrianSim): Force = {

    // parameters from original article (On modeling and evolutionary optimization of nonlinearly coupled pedestrian interactions)
    val A: Double = 0.8
    val B: Double = 0.62
    val lambda: Double = 0.19
    val dt: Double = 1.74 // [s]
    //val tau: Double = sim.dt // [s]

    // parameters from other optimization (Social force model with explicit collision prediction)
    //val A: Double = 1.33 // [m s^-2]
    //val B: Double = 0.34 // [m]
    //val lambda: Double = 0.08 // []
    //val tau: Double = 1.78 // [s]

    // vector from pedestrian2 pointing to pedestrian 1
    val dab: Direction = p1.currentPosition-p2.currentPosition
    val yab: Direction = dt * (p2.currentVelocity - p1.currentVelocity)
    val bab: Double = 0.5*sqrt(pow(norm(dab) + norm(dab - yab),2) - pow(norm(yab),2))

    // angle of sight reduction
    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1.0-lambda)*0.5*(1.0+desiredDirection.dot(dab/norm(dab)))
    //println(dab, yab, bab, desiredDirection,desiredDirection.dot(dab/norm(dab)),  w, exp(-bab/B), ((norm(dab) + norm(dab-yab))/2.0*bab) * 0.5 * (dab/norm(dab) + (dab-yab)/norm(dab-yab)))

    // final force
    w*A*exp((p1.r+p2.r-bab)/B) * ((norm(dab) + norm(dab-yab))/(4.0*bab)) * (dab/norm(dab) + (dab-yab)/norm(dab-yab))


    // distance between pedestrians
    //val distance: Double = breeze.linalg.norm(dab)


    /*val denominator: Double = 1.0 + tau * breeze.linalg.norm(p1.currentVelocity)
    val numeratorPart1: Double = distance + breeze.linalg.norm(dab - (p2.currentVelocity-p1.currentVelocity)*tau)
    val numeratorPart2: Double = breeze.linalg.norm((p2.currentVelocity-p1.currentVelocity)*tau)
    val b: Double = 0.5*sqrt((pow(numeratorPart1,2) - pow(numeratorPart2,2))/denominator)

    // angle of sight reduction
    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1-lambda)*0.5*(1.0+cos(desiredDirection.dot(dab) / distance))
    //sim.errorLogger.error(p1.ID + ", " + distance.toString + ", " + p1.currentVelocity.toString + ", " + p2.currentVelocity.toString + ", " + numeratorPart1.toString + ", " + numeratorPart2.toString + ", " + denominator.toString + ", " + b.toString + ", " + denominator.toString + ", " + ((distance + norm(d21 - y12)) / (4*b)).toString + ", " + d21.toString + ", " + distance.toString + ", " + (d21 - y12).toString + ", " + norm(d21 - y12).toString)
    w*(A*exp(-b/B)) * dab/distance*/

    // used by the interpretation of the model in (Social force model with explicit collision prediction)
    //val y12: Position = d21 - (p2.currentVelocity-p1.currentVelocity)*tau
    //if (norm(d21 - y12) > 0.0) w*(A*exp(-b/B))/sqrt(denominator) * (distance + norm(d21 - y12)) / (4*b) * (d21/distance + (d21 - y12)/norm(d21 - y12))
    //else w*(A*exp(-b/B))/sqrt(denominator) * distance / (4*b) * (d21/distance)
  }


  protected def insertNextEvent(): Unit = sim.insertEventWithDelay(sim.sf_dt) (new SocialForceESII(sim))
}

