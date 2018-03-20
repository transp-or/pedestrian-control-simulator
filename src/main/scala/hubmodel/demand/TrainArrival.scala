package hubmodel.demand

import hubmodel.{Action, NewTime, SFGraphSimulator, VertexRectangle}

class TrainArrival(train: Train, sim: SFGraphSimulator) extends Action {

  val alightingFlows: Iterable[PedestrianFlow_New_Parent] = sim.pedestrianFlows.flowsPTInduced.filter({
    case f: PedestrianFlowPT_New => !sim.timeTable.isOnSamePlatform(f.O, f.D)
    case _ => true
  })


  val totalDisembarkingFlows: Double = alightingFlows.foldRight(0.0)((f: PedestrianFlow_New_Parent, acc: Double) => acc + f.f)
  val durationDisembarking: Double = totalDisembarkingFlows/2.176 // 2*(2.7-1.1) * 0.68 theoretical maximum disembarking rate.

  val flows: Iterable[(VertexRectangle, VertexRectangle, Double)] = alightingFlows.flatMap(f => splitFractionsUniform(sim.conceptualNode2GraphNodes(f.O), sim.conceptualNode2GraphNodes(f.D), f.f))

  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": train arrival")

    flows.foreach(flow => sim.insertEventWithZeroDelay {
      new PedestrianGeneration(flow._1, flow._2, new NewTime(0.0), math.round(flow._3).toInt, sim)
    })
  }
}
