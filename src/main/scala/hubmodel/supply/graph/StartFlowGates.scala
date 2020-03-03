package hubmodel.supply.graph

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.ped.PedestrianNOMAD

/** Initialisation of the flow gates. The is the event inserted into the [[NOMADGraphSimulator.StartSim]] event.
  * The "first round" of the [[hubmodel.supply.FlowGate.ReleasePedestrian]] events are inserted;
  * these will call the next events.
  *
  * @param sim simulation environment
  */
class StartFlowGates(sim: NOMADGraphSimulator) extends Action {
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": started flow gates")
    sim.controlDevices.flowGates.foreach(fg => sim.insertEventWithZeroDelay(new fg.ReleasePedestrian(sim)))
  }

  type A = StartFlowGates

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = Some(new StartFlowGates(simulator))

}
