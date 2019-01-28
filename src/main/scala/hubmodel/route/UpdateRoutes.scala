package hubmodel.route

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD



class UpdateRoutes[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {
  override def execute(): Unit = {
    sim.population.filter(sim.intermediateDestinationReached).filterNot(_.isWaiting).foreach(p => sim.updateIntermediateDestination(p))
    sim.insertEventWithDelay(sim.route_dt)(new UpdateRoutes(sim))
  }

  override def toString: String = "UpdateRoutes"
}
