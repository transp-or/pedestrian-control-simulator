package hubmodel.route

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}


class UpdatePedestrianRoutes(sim: NOMADGraphSimulator) extends Action {


  override def execute(): Unit = {

    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": updating pedestrian routes")

    sim.population.filter(sim.intermediateDestinationReached).filterNot(_.isWaiting).foreach(p => sim.updateIntermediateDestination(p))
    sim.insertEventWithDelay(sim.updateRoutesInterval)(new UpdatePedestrianRoutes(sim))
  }

  type A = UpdatePedestrianRoutes

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
    Some(new UpdatePedestrianRoutes(simulator))
  }

  override def toString: String = "UpdateRoutes"
}
