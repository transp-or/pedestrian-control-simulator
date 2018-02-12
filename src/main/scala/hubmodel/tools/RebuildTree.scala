package hubmodel.tools

import hubmodel.{Action, NewTime, SFGraphSimulator}

/** Event which trigers the reconstruction from scratch of the m-tree which is used to find the neighbouring
  * pedestrians. The execute functions calls the method defined in the simulator which rebuilds the tree.
  *
  * @param sim simulator passed as argument
  */
class RebuildTree(sim: SFGraphSimulator) extends Action {

  /** Triggers the execution of the m-tree by calling the [[SFGraphSimulator.rebuildMTree()]] method.
    *
    */
  override def execute(): Unit = {

    // logs the event
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": rebuilding neighbour tree")

    // rebuilds the tree from scratch
    sim.rebuildMTree()

    // inserts new rebuild tree event
    sim.insertEventWithDelayNew(sim.rebuildTreeInterval.get)(new RebuildTree(sim))
  }

}
