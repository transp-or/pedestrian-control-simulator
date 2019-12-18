package hubmodel.demand

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.cells.{Rectangle, Vertex}

class ReleasePedPTInducedFlow[T <: PedestrianNOMAD](o: Vertex, sim: NOMADGraphSimulator[T]) extends Action {

  override def execute(): Unit = {
    if (sim.PTInducedFlows(o).nonEmpty) {
      val pedGen: CreatePedestrian[T] = sim.PTInducedFlows(o).samplePed
      val pedID: String = pedGen.execute()
      if (pedGen.isTransfer) {
        sim.transferringPassengers.add(pedID)
      }
      sim.insertEventWithDelay(Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / sim.PTInducedFlows(o).rate)))(new ReleasePedPTInducedFlow(o, sim))
    }
  }
}
