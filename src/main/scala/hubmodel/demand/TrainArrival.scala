package hubmodel.demand

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.demand.transit.Vehicle
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.TrainID_New
import tools.Time

import scala.reflect.ClassTag

class TrainArrival[T <: PedestrianNOMAD](train: Vehicle, tinf: Seq[PedestrianFlowPT_New], sim: NOMADGraphSimulator[T])(implicit tag: ClassTag[T]) extends Action {

  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": train arrival")
    (train.alightingPassengers.groupBy(v => v).map(kv => {
      PedestrianFlowPT_New(train.ID, kv._2.head, kv._2.size)
    }) ++ tinf)
      .flatMap(pedFlow => {
        pedFlow.D match {
          case t: TrainID_New => {
            splitFractionsUniform(sim.stop2Vertices(pedFlow.O), sim.stop2Vertices(pedFlow.D), pedFlow.f).map(d => (d._1, d._2, d._3, true))
          }
          case _ => {
            splitFractionsUniform(sim.stop2Vertices(pedFlow.O), sim.stop2Vertices(pedFlow.D), pedFlow.f).map(d => (d._1, d._2, d._3, false))
          }
        }
      })
      .foreach(flow => sim.insertEventWithZeroDelay {
        new PedestrianGenerationTINF(flow._1, flow._2, flow._4, new Time(0.0), math.round(flow._3).toInt, sim)
      })
  }

  type A = TrainArrival[P]

  override def deepCopy(simulator: NOMADGraphSimulator[P]): Option[A] = None //Some(new TrainArrival(this.train, ))

}
