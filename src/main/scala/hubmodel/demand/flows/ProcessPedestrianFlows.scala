package hubmodel.demand.flows

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlow_New, PedestrianGenerationOverInterval, splitFractionsUniform}
import hubmodel.ped.PedestrianNOMAD
import tools.Time

import scala.reflect.ClassTag

/** Insert the events which wil process and insert the pedestrians into the simulator.
  *
  * @param sim simulator containing the data
  */
class ProcessPedestrianFlows(pedestrianFlows: Iterable[PedestrianFlow_New], pedestrianFlowsFunction: Iterable[PedestrianFlowFunction_New], sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

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
          sim.insertEventWithZeroDelay(new PedestrianGenerationOverInterval(
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
        sim.insertEventAtAbsolute(flow.start)(new PedestrianGenerationNonHomogeneousRate(
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

type A = ProcessPedestrianFlows

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = Some(new ProcessPedestrianFlows(this.pedestrianFlows, this.pedestrianFlowsFunction, simulator))
}


