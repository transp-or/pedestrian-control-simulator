package hubmodel.route

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import tools.cells.Vertex


class UpdatePedestrianRoutes(sim: NOMADGraphSimulator, withInsert: Boolean = true) extends Action {


  override def execute(): Unit = {

    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": updating pedestrian routes")

    this.sim.controlDevices.amws.foreach(_.updateCosts(sim.currentTime))
    this.sim.graph.updateGraphCosts()

    val (intermediateDest, outsideZones) = sim.population.partition(sim.intermediateDestinationReached)

    val amwVerticesNames: Vector[String] = this.sim.controlDevices.amws.flatMap(w => Vector(w.firstVertex.name, w.secondVertex.name)).toVector

    intermediateDest.filterNot(_.isWaiting).foreach(p => sim.updateIntermediateDestination(this.sim.currentTime, p))
    outsideZones.filter(p => !p.isWaiting && amwVerticesNames.contains(p.nextZone.name) && !amwVerticesNames.contains(p.previousZone.name)).foreach(p => sim.graph.processRouteOutOfZones(this.sim.currentTime, p))

    if (withInsert) {
      sim.insertEventWithDelay(sim.updateRoutesInterval)(new UpdatePedestrianRoutes(sim))
    }
  }

  type A = UpdatePedestrianRoutes

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
    None //Some(new UpdatePedestrianRoutes(simulator))
  }

  override def toString: String = "UpdateRoutes"
}
