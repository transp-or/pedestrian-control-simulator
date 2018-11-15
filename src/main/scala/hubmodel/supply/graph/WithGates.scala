package hubmodel.supply.graph

import hubmodel.ped.{PedestrianNOMAD, PedestrianSim, PedestrianTrait}
import hubmodel.tools.cells.isInVertex

trait WithGates {

  /** Puts a pedestrian in the queue in front of a gate. This method is called by the movement model after each
    * iteration and checks whether the pedestians have entered the queue.
    *
    * @param flowGates Collection of flow gates in which to possibly enqueue pedestrian.
    * @param p         pedestrian to enqueue.
    */
  def enqueueInWaitingZone(flowGates: Iterable[FlowGate])(p: PedestrianSim): Unit = {
    val gate: Option[FlowGate] = flowGates.filterNot(p.freedFrom contains _.ID).find(fg => isInVertex(fg.startVertex)(p.currentPosition))
    if (gate.isDefined && !gate.get.pedestrianQueue.contains(p)) {
      p.isWaiting = true
      gate.get.pedestrianQueue.enqueue(p)
    }
  }

  def processIntermediateArrival(ped: PedestrianNOMAD): Unit

}