package hubmodel.route

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}


class UpdateRoutes(sim: NOMADGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.population.filter(sim.intermediateDestinationReached).filterNot(_.isWaiting).foreach(p => sim.updateIntermediateDestination(p))
    sim.insertEventWithDelay(sim.route_dt)(new UpdateRoutes(sim))
  }

  type A = UpdateRoutes

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
    Some(new UpdateRoutes(simulator))
  }

  override def toString: String = "UpdateRoutes"
}
