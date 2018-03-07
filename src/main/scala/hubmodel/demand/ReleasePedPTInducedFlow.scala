package hubmodel.demand

import java.util.concurrent.ThreadLocalRandom

import hubmodel.{Action, NewTime, SFGraphSimulator, VertexRectangle}

class ReleasePedPTInducedFlow(o: VertexRectangle, sim: SFGraphSimulator) extends Action {

  override def execute(): Unit = {
    if (sim.PTInducedFlows(o).nonEmpty) {
      sim.PTInducedFlows(o).samplePed.execute()
      sim.insertEventWithDelayNew(new NewTime(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / sim.PTInducedFlows(o).rate)))(new ReleasePedPTInducedFlow(o, sim))
    }
  }
}
