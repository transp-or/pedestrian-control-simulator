package hubmodel.supply.graph

import hubmodel.tools.cells.isInVertex
import hubmodel.ped.PedestrianSim

trait WithGates {

  /** Puts a pedestrian in the queue in front of a gate. This method is called by the social force model after each
    * iteration and checks whether the pedestians have entered the queue.
    *
    * @param p pedestrian to enqueue
    */
  def enqueueInWaitingZone(flowGates: Iterable[FlowGate])(p: PedestrianSim): Unit = {
    val gate: Option[FlowGate] = flowGates.filterNot(p.freedFrom contains _.ID).find(fg => isInVertex(fg.startVertex)(p.currentPosition))
    if (gate.isDefined && !gate.get.pedestrianQueue.contains(p)) {
      p.isWaiting = true
      gate.get.pedestrianQueue.enqueue(p)
    }
  }
}