package hubmodel.supply.graph

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.Rectangle

/** Implementation of moving walkways as an edge. This will be used for the route choice aspects.
  *
  * @param startVertex vertex at origin
  * @param endVertex   vertex at destination
  * @param capacity    capacity of the MV
  */
class MovingWalkway(override val startVertex: Rectangle, override val endVertex: Rectangle, val capacity: Double) extends MyEdge(startVertex, endVertex) {
  val speed: Double = 2.0

  override def clone(): MovingWalkway = new MovingWalkway(this.startVertex, this.endVertex, this.capacity)
}

/** Initialisation of the flow gates. The is the event inserted into the [[NOMADGraphSimulator.StartSim]] event.
  * The "first round" of the [[hubmodel.supply.FlowGate.ReleasePedestrian]] events are inserted;
  * these will call the next events.
  *
  * @param sim simulation environment
  */
class StartFlowGates[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action[T] {
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": started flow gates")
    sim.controlDevices.flowGates.foreach(fg => sim.insertEventWithZeroDelay(new fg.ReleasePedestrian(sim)))
  }
}
