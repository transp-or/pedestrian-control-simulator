package hubmodel.mvmtmodels

import breeze.numerics.exp
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel._
import hubmodel.ped.PedestrianSim


/**
  * Created by nicholas on 5/12/17.
  */
//http://pubsonline.informs.org/doi/pdf/10.1287/trsc.1040.0108
class SocialForceHelbing2005(sim: SFGraphSimulator) extends SocialForceLike(sim) with Action {

  /** Compute the force affecting the pedestrian. No need to pass pedestrian as argument as the function gets
    * the coordinate from the scope.
    *
    * @param pos point on the wall
    * @return force acting on the pedestrian from pos
    */
  protected override def pedestrian2WallForceNew(ped: PedestrianSim, pos: Position): Force = {
    // set of parameters used for calculating the repulsive effects
    val A: Double = 5.0
    val B: Double = 0.1

    val ALimit: Double = 0
    val BLimit: Double = 1.0//0.001

    val dab: Direction = ped.currentPosition - pos
    val dabNorm: Double = dab.norm

    (dab / dabNorm) * A * exp((ped.r - dabNorm) / B) + (dab / dabNorm) * ALimit * exp((ped.r - dabNorm) / BLimit) /* +
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
  protected override def pedestrian2PedestrianForceNew(p1: PedestrianSim, p2: PedestrianSim): Force = {
    val A1: Double = 7.0
    val B1: Double = 0.3
    val A2: Double = 3.0
    val B2: Double = 0.2
    val lambda: Double = 0.75

    val dab: Direction = p1.currentPosition - p2.currentPosition
    val dabNorm: Double = (dab+0.00005).norm//pow(dab(0)*dab(0)+dab(1)*dab(1)+0.0001,0.5)


    val desiredDirection: Direction = computeDirection(p1.currentPosition, p1.currentDestination)
    val w: Double = lambda + (1.0 - lambda) * 0.5 * (1.0 + desiredDirection.dot(dab / dabNorm))
    //w * A1 * exp((p1.r + p2.r - dabNorm) / B1) * (dab / dabNorm) + A2 * exp((p1.r + p2.r - dabNorm) / B2) * (dab / dabNorm)
     (dab / dabNorm) * A2 * exp((p1.r + p2.r - dabNorm) / B2)
  }

  protected def insertNextEvent(): Unit = sim.insertEventWithDelay(sim.sf_dt)(new SocialForceHelbing2005(sim))

}
