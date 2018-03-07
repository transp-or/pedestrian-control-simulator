package hubmodel.mgmt

import hubmodel.{Action, SFGraphSimulator}


/**
  * Created by nicholas on 5/13/17.
  */

/** Moves the [[hubmodel.supply.BinaryGate]] positions. Based on the function measuring the state of the system, this Class
  * will chnage the positions of the gates.
  *
  * @param sim simulation environment
  */
/*class UpdateBinaryGates(sim: SFGraphSimulator) extends Action {
  val densityThresHold: Double = 1.0
  def computeDensity: Double = {sim.densityHistory.last._2}

  /** Moves the binary gates. The set of edges is the clsoedEdges variable is updated. This is the variable which
    * is used by the [[hubmodel.route.UpdateRoutes]] event to compute the routes for each pedestrian.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time: " + sim.currentTime + ": moving gates")
    sim.graph.binaryGates.foreach(bg => bg.flowRate = {
      if (computeDensity >= densityThresHold) 0
      else bg.flowRate
    })
    sim.gatesHistory.append((sim.currentTime, sim.gateCollection.map(g => (g.ID, g.isOpen))))
  }
}*/

/** Controls the flows passing through the gates. The flow is updated based on the state evaluations.
  *
  * TODO: Update this function for using perimeter control ideas.
  *
  * @param sim simulation environment
  */
class UpdateFlowGates(sim: SFGraphSimulator) extends Action {
  val p: Double = 1
  val flowMin: Double = 0.2 // pax/m/s
  val flowMax: Double = 1 // pax/m/s
  val rhoCritical: Double = 1 // pax/m^2
  val densityThreshold: Double = (1 - math.pow(flowMin / flowMax, 1.0 / p)) * rhoCritical

  def computeCoefficient(downstreamDensity: Double): Double = {
    math.pow(1 - downstreamDensity / rhoCritical, p)
  }

  override def execute(): Unit = {
    sim.eventLogger.trace("time: " + sim.currentTime + ": updating flow gates")
    sim.graph.flowGates.foreach(fg => fg.flowRate = {
      if (sim.densityHistory.last._2 >= densityThreshold) flowMin
      else flowMax * fg.width * computeCoefficient(sim.densityHistory.last._2)
    })
  }
}


