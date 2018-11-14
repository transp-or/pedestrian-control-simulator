package hubmodel.demand.flows

import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.Time
import hubmodel.demand.CreatePedestrian
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.{StopID_New, TrainID_New}

import scala.reflect.ClassTag

class ProcessDisaggregatePedestrianFlows[T <: PedestrianNOMAD](eventCollection: Iterable[(String, String, Time)], sim: NOMADGraphSimulator[T])(implicit tag: ClassTag[T]) extends Action[T] {

  def execute(): Unit = {
    eventCollection
      .filter(ec => ((sim.startTime <= ec._3 && ec._3 <= sim.finalTime) || ec._3.value == math.pow(10, 8)) && ec._1 != "-1" && ec._2 != "-1" && sim.graph.vertexMap.keySet.contains(ec._1.drop(2)) && sim.graph.vertexMap.keySet.contains(ec._2.drop(2)))
      .foreach(ec => {
        if (ec._1.contains("z_") && ec._2.contains("z_")) {
          sim.insertEventAtAbsolute(ec._3)(new CreatePedestrian(sim.graph.vertexMap(ec._1.drop(2)), sim.graph.vertexMap(ec._2.drop(2)), sim))
        } else if (ec._1.contains("t_") && ec._2.contains("z_")) {
          sim.timeTable.timeTable.find(_._1.ID == ec._1.drop(2)).get._2.addAlightingPassenger(StopID_New(ec._2.drop(2).toInt, ""))
        } else if (ec._1.contains("t_") && ec._2.contains("t_")) {
          sim.timeTable.timeTable.find(_._1.ID == ec._1.drop(2)).get._2.addAlightingPassenger(TrainID_New(ec._2.drop(2), ""))
        } else if (ec._1.contains("z_") && ec._2.contains("t_")) {
          sim.insertEventAtAbsolute(ec._3)(new CreatePedestrian(sim.graph.vertexMap(ec._1.drop(2)), sim.graph.vertexMap(ec._2.drop(2)), sim))
        } else {
          sim.insertEventAtAbsolute(ec._3)(new CreatePedestrian(sim.graph.vertexMap(ec._1.drop(2)), sim.graph.vertexMap(ec._2.drop(2)), sim))
        }
      })
  }

}