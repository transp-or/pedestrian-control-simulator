package hubmodel.demand

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel._

class ReleasePedPTInducedFlow(o: Vertex, sim: SFGraphSimulator) extends Action {

  override def execute(): Unit = {
    if (sim.PTInducedFlows(o).nonEmpty) {
      sim.PTInducedFlows(o).samplePed.execute()
      sim.insertEventWithDelayNew(Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / sim.PTInducedFlows(o).rate)))(new ReleasePedPTInducedFlow(o, sim))
    }
  }
}
