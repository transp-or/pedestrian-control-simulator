package hubmodel.control.flowgate

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.{P, Position, GATE_MAXIMUM_QUEUE_SIZE}
import hubmodel.control.ControlDeviceComponent
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}
import hubmodel.supply.graph.MyEdgeWithGate
import tools.Time
import tools.cells.{Rectangle, Vertex}
import tools.exceptions.ControlDevicesException

/** Extension of [[hubmodel.supply.graph.MyEdgeWithGate]] for the usage of "flow gates". The gates control the
  * flow of pedestrians passing through them.
  *
  * TODO: A more realistic approach for the modeling of gates could be used to determine the maximum capacity.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param start       one end of the gate
  * @param end         other end of the gate
  */
class FlowGate(startVertex: Vertex, endVertex: Vertex, start: Position, end: Position, ma: String) extends MyEdgeWithGate(startVertex, endVertex, start, end, ma) with ControlDeviceComponent {

  // self-type allowing access to contents of this class from inner classes.
  gate =>

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: FlowGate => super.equals() && that.canEqual(this) && this.start == that.start && this.end == that.end
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other
    * @return
    */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FlowGate]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (super.hashCode, this.start, this.end).##
  }

  override def toString: String = "FlowGate: o: " + startVertex + ", d:" + endVertex

  override def deepCopy: FlowGate = new FlowGate(
    this.startVertex, this.endVertex, this.start, this.end, this.ma
  )

  def deepCopyWithState(t: Time, pop: Iterable[PedestrianNOMAD]): FlowGate = {
    val fg = new FlowGate( this.startVertex, this.endVertex, this.start, this.end, this.ma )
    fg.setFlowRate(this.flowRate, this.positionHistory.last._1)
    fg.pedestrianQueue.enqueueAll(this.pedestrianQueue.map(p => pop.find(_.ID == p.ID).getOrElse(throw new Exception("Pedestrian missing ! s"))))
    fg
  }


  /** Event for releasing a pedestrian. This allows him to pass the gate. Each pedestrian contains state variables
    * indicating whether he is waiting or not. These are used by the other classes.
    *
    * @param sim simulation environment
    */
  class ReleasePedestrian(sim: NOMADGraphSimulator) extends Action {

    /** Executes the event.
      *
      */
    override def execute(): Unit = {
      if (pedestrianQueue.nonEmpty) {
        pedestrianQueue.head.isWaiting = false
        pedestrianQueue.head.freedFrom.append(ID)
        pedestrianQueue.dequeue()
        sim.eventLogger.trace("sim-time=" + sim.currentTime + ": gate: " + gate.ID + " released pedestrian. Peds in queue=" + pedestrianQueue.size)
      } else {
        sim.eventLogger.trace("sim-time=" + sim.currentTime + ": gate: " + gate.ID + ": no one in queue to release")
      }
      // inserts new event based on the current flow rate allowed through the gate.
      //sim.insertEventWithDelay(1.0 / flowRate)(new ReleasePedestrian(sim))
    }

    type A = ReleasePedestrian

    override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = { ???  }

  }



  /** Enqueues a pedestrian i the queue for passing through a flow gate.
    *
    * @param ped pedestrian to enqueue
    * @param sim simulator for getting the logger and other elements.
    */
  class EnqueuePedestrian(ped: PedestrianSim, sim: NOMADGraphSimulator) extends Action {

    override def execute(): Unit = {

      ped.isWaiting = true
      pedestrianQueue.enqueue(ped)
      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": enqueued pedestrian in " + gate.ID + ". # peds in queue: " + pedestrianQueue.size)
      if (gate.pedestrianQueue.size > GATE_MAXIMUM_QUEUE_SIZE) {
        throw new ControlDevicesException("Too many pedestrians in queue for gate " + gate.ID)
      }
    }

    type A = EnqueuePedestrian

    override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = { ???  }
  }
}