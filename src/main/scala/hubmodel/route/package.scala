package hubmodel

import hubmodel.ped.PedestrianSim
import hubmodel.supply.graph.RouteGraph

package object route {

  def processIntermediateArrival(graph: RouteGraph)(p: PedestrianSim): Unit = {
      if (graph.isFloorChange(p.nextZone, p.route.head)) {
        p.previousZone = p.route.head
        p.currentPosition = p.route.head.uniformSamplePointInside
        p.nextZone = p.route.tail.head
        p.route = graph.getShortestPath(p.nextZone, p.finalDestination).tail
      }
      else {
        p.previousZone = p.nextZone

        p.route = graph.getShortestPath(p.nextZone, p.finalDestination).tail
        p.nextZone = p.route.head
      }
      p.setCurrentDestination(p.nextZone.uniformSamplePointInside)
  }
}
