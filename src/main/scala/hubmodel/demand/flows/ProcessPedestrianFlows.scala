package hubmodel.demand.flows

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlow_New, PedestrianGenerationOverInterval, splitFractionsUniform}
import hubmodel.ped.PedestrianNOMAD
import tools.Time

import scala.reflect.ClassTag

/** Insert the arrivals of all vehicle in the event list. The trains variables stored in the simulation is the
  * variable which is used.
  *
  * @param sim simulator containing the data
  */
class ProcessPedestrianFlows[T <: PedestrianNOMAD](pedestrianFlows: Iterable[PedestrianFlow_New], pedestrianFlowsFunction: Iterable[PedestrianFlowFunction_New], sim: NOMADGraphSimulator[T])(implicit tag: ClassTag[T]) extends Action {

  /**
    * Process the flows defined from zone to zone (not linked to PT). The flows are distributed between the
    * different conceptual zones to the physical zones.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": inserting pedestrian flows")

    // uniform pedestrian flows
    pedestrianFlows
      .foreach(flow => splitFractionsUniform(sim.stop2Vertices(flow.O), sim.stop2Vertices(flow.D), flow.f)
        .foreach(f => {
          sim.insertEventWithZeroDelay(new PedestrianGenerationOverInterval[T](
            f._1,
            f._2,
            flow.start,
            flow.end,
            math.round(f._3).toInt,
            sim
          ))
        }))

    // functional pedestrian flows
    pedestrianFlowsFunction
      .foreach(flow => splitFractionsUniform(sim.stop2Vertices(flow.O), sim.stop2Vertices(flow.D)).foreach(f => {
        sim.insertEventAtAbsolute(flow.start)(new PedestrianGenerationNonHomogeneousRate[T](
          f._1,
          f._2,
          flow.start,
          flow.end,
          (t: Time) => flow.f(t) * f._3,
          sim
        ))
      }))
  }

  /**
    * Defines the name of the action.
    *
    * @return string contaiing the name.
    */
  override def toString: String = "ProcessPedestrianFlows"
}


