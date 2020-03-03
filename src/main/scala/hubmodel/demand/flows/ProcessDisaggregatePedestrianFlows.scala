package hubmodel.demand.flows

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import hubmodel.demand.CreatePedestrian
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.{StopID_New, TrainID_New}
import tools.Time

import scala.reflect.ClassTag

class ProcessDisaggregatePedestrianFlows(eventCollection: Iterable[(String, String, Option[Time])], sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

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
      } else if (ec._1.contains("t_") && ec._2.contains("z_")) {
        sim.timeTable.get.timeTable.find(_._1.ID == ec._1.drop(2)).get._2.addAlightingPassenger(StopID_New(ec._2.drop(2), ""))
      } else if (ec._1.contains("t_") && ec._2.contains("t_")) {
        if (sim.timeTable.get.timeTable.exists(_._1.ID == ec._1.drop(2)) && sim.timeTable.get.timeTable.exists(_._1.ID == ec._2.drop(2))) {
          sim.timeTable.get.timeTable.find(_._1.ID == ec._1.drop(2)).get._2.addAlightingPassenger(TrainID_New(ec._2.drop(2), ""))
        } else {
          throw new Exception("Pedestrian's vehicle does not exist ! O=" + ec._1 + ", D=" + ec._2)
        }
      } else if (ec._1.contains("z_") && ec._2.contains("t_")) {
        val possibleZoneIDs: Seq[String] = sim.stop2Vertices(TrainID_New(ec._2.drop(2), "")).map(_.name).toVector
        sim.insertEventAtAbsolute(ec._3.get)(new CreatePedestrian(
          sim.graph.vertexMapNew(ec._1.drop(2)),
          sim.graph.vertexMapNew(possibleZoneIDs(ThreadLocalRandom.current.nextInt(possibleZoneIDs.size))),
          false,
          sim))
      } else {
        throw new Exception("Case not covered")
      }
    })
  }

  type A = ProcessDisaggregatePedestrianFlows

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = None // Some(new ProcessDisaggregatePedestrianFlows(this.eventCollection, simulator))

}
