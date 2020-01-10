package hubmodel.demand

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.cells.{Rectangle, Vertex}
import hubmodel.P

/** Release a pedestrian from a specific queue. The execute method:
  *  - samples the pedestrian to add from the queue and executes the insert action
  *  - adds a new [[ReleasePedPTInducedFlow]] event later based on the release rate
  *
  * @param o vertex from which to release a pedestrian
  * @param sim simulator
  * @tparam T pedestrian type
  */
class ReleasePedPTInducedFlow[T <: P](o: Vertex, sim: NOMADGraphSimulator[T]) extends Action {

  /** executes the [[ReleasePedPTInducedFlow]] event by adding a pedestrian into the simulator and then
    * adds another [[ReleasePedPTInducedFlow]] event into the DES queue.
    *
    */
  override def execute(): Unit = {
    if (sim.PTInducedFlows(o).nonEmpty) {

      // creation event to execute
      val pedGen: CreatePedestrian[T] = sim.PTInducedFlows(o).samplePed
      val pedID: String = pedGen.execute()

      // stores thes IDs of transferring pedestrians
      if (pedGen.isTransfer) {
        sim.transferringPassengers.add(pedID)
      }

      // next event added into the queue
      sim.insertEventWithDelay(Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / sim.PTInducedFlows(o).rate)))(new ReleasePedPTInducedFlow(o, sim))
    }
  }

   type A = ReleasePedPTInducedFlow[P]

  override def deepCopy(simulator: NOMADGraphSimulator[P]): Option[A] = Some(new ReleasePedPTInducedFlow[P](this.o, simulator))
}
