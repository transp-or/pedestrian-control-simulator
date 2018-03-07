package hubmodel.demand

/**
  * Created by nicholas on 5/12/17.
  */

import hubmodel.{Action, NewTime, SFGraphSimulator}

/** Insert the arrivals of all vehicle in the event list. The trains variables stored in the simulation is the
  * variable which is used.
  *
  * @param sim simulator containing the data
  */
class ProcessTimeTable(sim: SFGraphSimulator) extends Action {

  /**
    * Execution of the event.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": inserting vehicles")
    sim.timeTable.trains.filter(t => t._2.arr.isDefined).foreach(t => sim.insertEventAtAbsolute(NewTime(t._2.arr.get.toSecondOfDay)) {
      new TrainArrival(t._2, sim)
    })
  }
}


