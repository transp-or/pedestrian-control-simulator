package hubmodel.demand

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.{Action, SFGraphSimulator}

/** Insert the arrivals of all vehicle in the event list. The trains variables stored in the simulation is the
  * variable which is used.
  *
  * @param sim simulator containing the data
  */
class ProcessPedestrianFlows(sim: SFGraphSimulator) extends Action {

  /**
    * Execution of the event.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": inserting pedestrian flows")
    sim.pedestrianFlows.flows
      .foreach(flow => splitFractionsUniform(sim.conceptualNode2GraphNodes(flow.O), sim.conceptualNode2GraphNodes(flow.D), flow.f)
        .foreach(f => {sim.insertEventWithZeroDelay(new PedestrianGenerationOverInterval(
          f._1,
          f._2,
          flow.start,
          flow.end,
          math.round(f._3).toInt,
          sim
    ))}))
  }
}


