package hubmodel.supply.graph

import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel.Position
import hubmodel.ped.PedestrianSim
import hubmodel.tools.cells.Rectangle

abstract class MyEdgeWithGate(override val startVertex: Rectangle, override val endVertex: Rectangle, val start: Position, val end: Position, val monitoredArea: String) extends MyEdge(startVertex, endVertex) {

  val width: Double = (end - start).norm

  // variable flow rate of the gate [pax/s]
  var flowRate: Double

  // The list of pedestrians which are waiting at the gate to be let through
  val pedestrianQueue: scala.collection.mutable.Queue[PedestrianSim] = scala.collection.mutable.Queue[PedestrianSim]()

  /** Event for releasing a pedestrian. This allows him to pass the gate. Each pedestrian contains state variables
    * indicating whether he is waiting or not. These are used by the other classes.
    *
    * @param sim simulation environment
    */
  class ReleasePedestrian(sim: SFGraphSimulator) extends Action {

    /** Executes the event.
      *
      */
    override def execute(): Unit = {
      if (pedestrianQueue.nonEmpty) {
        pedestrianQueue.head.isWaiting = false
        pedestrianQueue.head.freedFrom.append(ID)
        pedestrianQueue.dequeue()
        sim.eventLogger.trace("sim-time=" + sim.currentTime + ": gate: " + this + ": released pedestrian. Peds in queue=" + pedestrianQueue.size)
      } else {
        sim.eventLogger.trace("sim-time=" + sim.currentTime + ": gate: " + this + ": no one in queue to release")
      }
      // inserts new event based on the current flow rate allowed through the gate.
      //sim.insertEventWithDelay(1.0 / flowRate)(new ReleasePedestrian(sim))
    }
  }

  /** Enqueues a pedestrian i the queue for passing through a flow gate.
    *
    * @param sim
    */
  class EnqueuePedestrian(ped: PedestrianSim, sim: SFGraphSimulator) extends Action {

    override def execute(): Unit = {
      ped.isWaiting = true
      pedestrianQueue.enqueue(ped)
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": enqueued pedestrian. Peds in queue=" + pedestrianQueue.size)
    }
  }

}