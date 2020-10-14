package hubmodel.demand

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import tools.Time
import tools.cells.Vertex

/** Release a pedestrian from a specific queue. The execute method:
  *  - samples the pedestrian to add from the queue and executes the insert action
  *  - adds a new [[ReleasePedPTInducedFlow]] event later based on the release rate
  *
  * @param o vertex from which to release a pedestrian
  * @param sim simulator
  */
class ReleasePedPTInducedFlow(o: Vertex, sim: NOMADGraphSimulator) extends Action {

  /** executes the [[ReleasePedPTInducedFlow]] event by adding a pedestrian into the simulator and then
    * adds another [[ReleasePedPTInducedFlow]] event into the DES queue.
    *
    */
  override def execute(): Unit = {
    if (sim.PTInducedFlows(o).nonEmpty) {

      // creation event to execute
      val pedGen: CreatePedestrian = sim.PTInducedFlows(o).samplePed
      val pedID: String = pedGen.execute()

      // stores thes IDs of transferring pedestrians
      if (pedGen.isTransfer) {
        sim.transferringPassengers.add(pedID)
      }

      // next event added into the queue
      sim.insertEventWithDelay(Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / sim.PTInducedFlows(o).rate)))(new ReleasePedPTInducedFlow(o, sim))
    }
  }

   type A = ReleasePedPTInducedFlow

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = Some(new ReleasePedPTInducedFlow(this.o, simulator))
}
