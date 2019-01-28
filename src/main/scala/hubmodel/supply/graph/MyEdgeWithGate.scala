package hubmodel.supply.graph

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}
import hubmodel.tools.cells.Rectangle
import hubmodel.{Position, Time}

abstract class MyEdgeWithGate(override val startVertex: Rectangle, override val endVertex: Rectangle, val start: Position, val end: Position, val monitoredArea: String) extends MyEdge(startVertex, endVertex) {

  // Physical width of the gate. Required to compute the maximum flow.
  val width: Double = (end - start).norm

  // private container for the flow rate of the gate [pax/s]
  private var _flowRate: Double = this.width * 1.0

  // Container for storing the states of the flow gates
  val positionHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()


  // The list of pedestrians which are waiting at the gate to be let through
  val pedestrianQueue: scala.collection.mutable.Queue[PedestrianSim] = scala.collection.mutable.Queue[PedestrianSim]()


  /**
    * Getter method for the flow rate
    *
    * @return flow rate
    */
  def flowRate: Double = this._flowRate

  /**
    * Updates the flow rate.
    *
    * @param newFlowRate new flow rate
    */
  @Deprecated
  def setFlowRate(newFlowRate: Double): Unit = {
    this._flowRate = newFlowRate
  }

  /**
    * Updates the flow rate and appends the value to the history of positions of the gates.
    *
    * @param newFlowRate new flow rate
    * @param currentTime start of the time interval for which this flow rates is valid
    */
  def setFlowRate(newFlowRate: Double, currentTime: Time): Unit = {
    this._flowRate = newFlowRate
    this.positionHistory.append((currentTime, newFlowRate))
  }


  /** Event for releasing a pedestrian. This allows him to pass the gate. Each pedestrian contains state variables
    * indicating whether he is waiting or not. These are used by the other classes.
    *
    * @param sim simulation environment
    */
  class ReleasePedestrian[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {

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
    * @param ped pedestrian to enqueue
    * @param sim simulator for getting the logger and other elements.
    */
  class EnqueuePedestrian[T <: PedestrianNOMAD](ped: PedestrianSim, sim: NOMADGraphSimulator[T]) extends Action {

    override def execute(): Unit = {
      ped.isWaiting = true
      pedestrianQueue.enqueue(ped)
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": enqueued pedestrian. Peds in queue=" + pedestrianQueue.size)
    }
  }

}