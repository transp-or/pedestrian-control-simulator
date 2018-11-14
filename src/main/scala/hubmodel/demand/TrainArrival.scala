package hubmodel.demand

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel._
import hubmodel.demand.transit.Vehicle
import hubmodel.ped.PedestrianNOMAD

import scala.reflect.ClassTag

class TrainArrival[T <: PedestrianNOMAD](train: Vehicle, tinf: Iterable[PedestrianFlowPT_New], sim: NOMADGraphSimulator[T])(implicit tag: ClassTag[T]) extends Action[T] {

  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": train arrival")
    tinf.flatMap(pedFlow => splitFractionsUniform(sim.stop2Vertices(pedFlow.O), sim.stop2Vertices(pedFlow.D), pedFlow.f))
      .foreach(flow => sim.insertEventWithZeroDelay {
      new PedestrianGeneration(flow._1, flow._2, new Time(0.0), math.round(flow._3).toInt, sim)
    })
  }
}
