package hubmodel.prediction

import hubmodel.DES.{NOMADGraphSimulator, PedestrianPrediction, PedestrianSimulation}
import hubmodel.P
import hubmodel.ped.PedestrianSim
import hubmodel.prediction.state.{CurrentState, StateGroundTruth}
import hubmodel.supply.graph.GraphContainer
import tools.Time

/** Prediction using the ground truth simulator. This class makes a copy of the state of the simulation and then
  * performs a simulation to predict the upcoming state.
  *
  * @param sim
  */
class PredictWithGroundTruth(sim: PedestrianSimulation) extends StatePrediction {

  val predictionHorizon: Time = this.sim.evaluate_dt*3.0

  private val predictionSimulator: PedestrianPrediction = {

    val stateData: StateGroundTruth = this.getActualStateData

    val graph: GraphContainer = sim.graph.deepCopy(stateData.controlDevices)


    val simulator: PedestrianPrediction = new PedestrianPrediction(
      sim.currentTime,
      sim.currentTime + predictionHorizon ,
      sf_dt = sim.sf_dt,
      route_dt = sim.route_dt,
      evaluate_dt = sim.evaluate_dt,
      rebuildTreeInterval = Some(sim.rebuildTreeInterval.get),
      spaceMicro = sim.spaceMicro,
      graph = graph,
      timeTable = sim.timeTable,
      stop2Vertex = sim.stop2Vertex,
      controlDevices = stateData.controlDevices,
      sim.logFullPedestrianHistory)

    // Insertion of the population
    stateData.population.foreach(simulator.insertInPopulation)

    // clone the event list into the new simulator
    sim.cloneEventQueue(simulator)


    simulator
  }

  /** Collect the state data from the reference simulator which will be used as starting point for the prediction.
    *
    * @return
    */
  protected def getActualStateData: StateGroundTruth = {
    val pop = sim.population.map(p => p.copyState(sim.currentTime, false)).toVector
    new StateGroundTruth(pop, sim.controlDevices.deepCopyWithState(sim.currentTime, pop))
  }

  /** Performs the prediction by running the simulator.
    *
    */
  def predict(): Unit = {
    this.predictionSimulator.run()
  }

  def getPredictedStateData: StateGroundTruth = {

    new StateGroundTruth(
      (this.predictionSimulator.population ++ this.predictionSimulator.populationCompleted).toVector,
      this.predictionSimulator.controlDevices
    )
  }

}
