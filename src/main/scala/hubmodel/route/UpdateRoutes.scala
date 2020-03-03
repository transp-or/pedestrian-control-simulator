package hubmodel.route

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.ped.PedestrianNOMAD


class UpdateRoutes(sim: NOMADGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.population.filter(sim.intermediateDestinationReached).filterNot(_.isWaiting).foreach(p => sim.updateIntermediateDestination(p))
    sim.insertEventWithDelay(sim.route_dt)(new UpdateRoutes(sim))
  }

  type A = UpdateRoutes

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = {
    Some(new UpdateRoutes(simulator))
  }

  override def toString: String = "UpdateRoutes"
}
