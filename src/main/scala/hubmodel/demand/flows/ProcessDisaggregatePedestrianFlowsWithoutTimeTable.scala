package hubmodel.demand.flows

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import hubmodel.demand.CreatePedestrian
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.cells.Vertex

import scala.reflect.ClassTag

class ProcessDisaggregatePedestrianFlowsWithoutTimeTable(eventCollection: Iterable[(String, String, Option[Time])], sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

  def execute(): Unit = {
    eventCollection
      .filter(ec => ec._1 != ec._2) // && ec._1 != "-1" && ec._2 != "-1" && sim.graph.vertexMapNew.keySet.contains(ec._1.drop(2)) && sim.graph.vertexMapNew.keySet.contains(ec._2.drop(2)))
      .foreach(ec => {
      if (ec._1.contains("z_") && ec._2.contains("z_")) {
        if (ec._1.drop(2) != "-1" && ec._2.drop(2) != "-1") {
          // update origin if it has been split into different zones
          val equivalentOrigins: Vector[Vertex] = this.sim.graph.destination2EquivalentDestinationsFunc(ec._1.drop(2))
          val totalAreaO: Double = equivalentOrigins.map(_.area).sum
          val rO: Double = ThreadLocalRandom.current().nextDouble()
          val origin = equivalentOrigins(equivalentOrigins.map(_.area / totalAreaO).scanLeft(0.0)(_ + _).zipWithIndex.takeWhile(_._1 < rO).last._2)

          // update destination if it has been split into different zones
          val equivalentDestinations: Vector[Vertex] = this.sim.graph.destination2EquivalentDestinationsFunc(ec._2.drop(2))
          val totalAreaD: Double = equivalentDestinations.map(_.area).sum
          val rD: Double = ThreadLocalRandom.current().nextDouble()
          val destination = equivalentDestinations(equivalentDestinations.map(_.area / totalAreaD).scanLeft(0.0)(_ + _).zipWithIndex.takeWhile(_._1 < rD).last._2)

          sim.insertEventAtAbsolute(ec._3.get)(new CreatePedestrian(origin, destination, false, sim))
        } else {
          //errorLogger.warn("Pedestrian dropped since invalid OD: " + ec)
        }
      } else {
        throw new Exception("This case should not happen ! ")
      }
    })
  }

  type A = ProcessDisaggregatePedestrianFlowsWithoutTimeTable

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
    Some(new ProcessDisaggregatePedestrianFlowsWithoutTimeTable(this.eventCollection, simulator))
  }

}
