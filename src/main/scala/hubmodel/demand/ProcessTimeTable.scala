package hubmodel.demand

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import hubmodel.ped.PedestrianNOMAD

import scala.reflect.ClassTag

/** Insert the arrivals of all vehicle in the event list. The trains variables stored in the simulation is the
  * variable which is used.
  *
  * @param sim simulator containing the data
  */
class ProcessTimeTable(timeTable: PublicTransportSchedule, PTInducedFlows: Seq[PedestrianFlowPT_New], sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

  /**
    * Execution of the event.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": inserting vehicles")
    timeTable.timeTable
      .filter(t => t._2.arr.isDefined)
      .foreach(t => sim.insertEventAtAbsolute(t._2.arr.get) {
        new TrainArrival(t._2, PTInducedFlows.filter(tinf => tinf.O == t._1), sim)
      })
  }

  type A = ProcessTimeTable

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = None //Some(new ProcessTimeTable(timeTable, PTInducedFlows, simulator))

  override def toString: String = "ProcessTimeTable"
}


