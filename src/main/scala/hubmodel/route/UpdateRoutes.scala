package hubmodel.route

import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel.tools.cells.Rectangle


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
      /*val closestNode: Rectangle = sim.graph.vertexMap.values.map(r => (r, (r.center - p.currentPosition).norm)).minBy(_._2)._1
      val newRoute: List[Rectangle] = {
        val route: List[Rectangle] = sim.graph.getShortestPath(closestNode, p.finalDestination)
        if (route.head.isInside(p.currentPosition)) route.tail else route
      }*/
      p.previousZone = p.nextZone
      val newRoute: List[Rectangle] = sim.graph.getShortestPath(p.nextZone, p.finalDestination).tail
      /*if (sim.closedEdges.exists(ce => ce._1 == p.nextZone && ce._2 == newRoute.head)) {
        p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
        println("in useless if ?")
      } else {*/
        p.route = newRoute
        p.nextZone = newRoute.head
        p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
      //}
    })
    sim.insertEventWithDelayNew(sim.sf_dt)(new UpdateRoutes(sim))
  }

  override def toString: String = "UpdateRoutes"
}

/*class UpdateGraph(sim: SFGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.graph.updateGraph()
  }
}*/
