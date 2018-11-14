package hubmodel.demand

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.{NodeParent, TrainID_New}

import scala.reflect.ClassTag

/** Insert the arrivals of all vehicle in the event list. The trains variables stored in the simulation is the
  * variable which is used.
  *
  * @param sim simulator containing the data
  */
class ProcessTimeTable[T <: PedestrianNOMAD](timeTable: PublicTransportSchedule, PTInducedFlows: Iterable[PedestrianFlowPT_New], sim: NOMADGraphSimulator[T])(implicit tag: ClassTag[T]) extends Action[T] {

  /**
    * Execution of the event.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": inserting vehicles")
    timeTable.timeTable
      .filter(t => t._2.arr.isDefined)
      .foreach(t => sim.insertEventAtAbsolute(t._2.arr.get) { new TrainArrival(t._2, PTInducedFlows.filter(tinf => tinf.O == t._1), sim) })
  }

  override def toString: String = "ProcessTimeTable"
}


