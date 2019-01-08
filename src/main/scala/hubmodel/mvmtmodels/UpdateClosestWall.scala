package hubmodel.mvmtmodels

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.mvmtmodels.NOMAD.getClosestPoint

class UpdateClosestWall[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {

  /** Updates the list of closest walls for each pedestrian.
    *
    */
  override def execute(): Unit = {


    // logs the event
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": updating closest walls for all pedestrians")

    sim.population.foreach(p => p.updateClosestWalls(sim.walls))

    // inserts new rebuild tree event
    sim.insertEventWithDelay(sim.rebuildTreeInterval.get)(new UpdateClosestWall(sim))
  }

}
