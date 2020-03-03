package hubmodel.demand.flows

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.demand.CreatePedestrian
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.{StopID_New, TrainID_New}
import tools.Time

import scala.reflect.ClassTag

class ProcessDisaggregatePedestrianFlowsWithoutTimeTable(eventCollection: Iterable[(String, String, Option[Time])], sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

  def execute(): Unit = {
    eventCollection
      .filter(ec => ec._1 != ec._2) // && ec._1 != "-1" && ec._2 != "-1" && sim.graph.vertexMapNew.keySet.contains(ec._1.drop(2)) && sim.graph.vertexMapNew.keySet.contains(ec._2.drop(2)))
      .foreach(ec => {
      if (ec._1.contains("z_") && ec._2.contains("z_")) {
        if (ec._1.drop(2) != "-1" && ec._2.drop(2) != "-1") {
          sim.insertEventAtAbsolute(ec._3.get)(new CreatePedestrian(sim.graph.vertexMapNew(ec._1.drop(2)), sim.graph.vertexMapNew(ec._2.drop(2)), false, sim))
        } else {
          //errorLogger.warn("Pedestrian dropped since invalid OD: " + ec)
        }
      } else {
        throw new Exception("This case should not happen ! ")
      }
    })
  }

  type A = ProcessDisaggregatePedestrianFlowsWithoutTimeTable

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = {
    Some(new ProcessDisaggregatePedestrianFlowsWithoutTimeTable(this.eventCollection, simulator))
  }

}
