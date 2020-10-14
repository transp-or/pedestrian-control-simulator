package hubmodel.mvmtmodels

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}

/** Event which trigers the reconstruction from scratch of the m-tree which is used to find the neighbouring
  * pedestrians. The execute functions calls the method defined in the simulator which rebuilds the tree.
  *
  * @param sim simulator passed as argument
  */
class RebuildPopulationTree(sim: NOMADGraphSimulator) extends Action {

  /** Triggers the execution of the m-tree by calling the [[NOMADGraphSimulator.rebuildMTree()]] method.
    *
    */
  override def execute(): Unit = {

    // logs the event
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": rebuilding neighbour tree")

    // rebuilds the tree from scratch
    sim.rebuildMTree()

    sim.population.foreach(ped => {
      ped.setClosePeds(sim.findNeighbours(ped.ID, sim.rebuildTreeInterval.get.value.toDouble * 8.0))
      //ped.closePeds = sim.population
      //  println("pause")
    })

    // inserts new rebuild tree event
    sim.insertEventWithDelay(sim.rebuildTreeInterval.get)(new RebuildPopulationTree(sim))
  }


  type A = RebuildPopulationTree

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = {
    None // Some(new RebuildPopulationTree(simulator))
  }


}
