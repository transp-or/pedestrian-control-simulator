package hubmodel.route

import hubmodel.VertexCell
import hubmodel.{Action, PedestrianDES, PedestrianSim, SFGraphSimulator}


/**
  * Created by nicholas on 5/29/17.
  */

/*class UpdateRouteForPedestrian(p: PedestrianSim, sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": updating route for pedestrian=" + p.ID)
    val newRoute: List[MyVertex] = sim.infraGraph.graph.getShortestPathFunction(p.nextZone, sim.infraGraph.graph.vertexMap(p.dZone.toString)).tail
    if (sim.closedEdges.exists(ce => ce._1 == p.nextZone && ce._2 == newRoute.head)) {
      p.currentDestination = sim.infraGraph.generateInZone(p.nextZone.name)
    } else {
      p.route = newRoute
      p.nextZone = newRoute.head
      p.currentDestination = sim.infraGraph.generateInZone(p.nextZone.name)
    }
  }
}*/

class UpdateRoutes(sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.population.filter(sim.intermediateDestinationReached).filterNot(_.isWaiting).foreach(p => {
      val newRoute: List[VertexCell] = sim.graph.getShortestPath(p.nextZone, sim.graph.vertexMap(p.dZone.toString)).tail
      if (sim.closedEdges.exists(ce => ce._1 == p.nextZone && ce._2 == newRoute.head)) {
        p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
      } else {
        p.route = newRoute
        p.nextZone = newRoute.head
        p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
      }
    })
    sim.insertEventWithDelay(sim.sf_dt)(new UpdateRoutes(sim))
  }
}

class UpdateGraph(sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.graph.updateGraph()
  }
}
