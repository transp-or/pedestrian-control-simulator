package hubmodel.prediction

import hubmodel.DES.{PedestrianPrediction, PedestrianSimulation, SimulationInputParameters}
import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import hubmodel.io.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.prediction.state.{StateGroundTruth, StateGroundTruthPredicted}
import hubmodel.supply.continuous.MovableWall
import hubmodel.supply.graph.GraphContainer
import tools.Time
import tools.TimeNumeric.mkOrderingOps

/** Prediction using the ground truth simulator. This class makes a copy of the state of the simulation and then
  * performs a simulation to predict the upcoming state.
  *
  * @param sim
  */
class PredictWithGroundTruth(private val sim: PedestrianSimulation, predictionHorizon: Time, val predictionInterval: Time, densityUpdateInterval: Time) extends StatePrediction {



  private var predictionSimulator: PedestrianPrediction = this.buildPredictionSimulator

  val predictionStartTime: Time = this.predictionSimulator.startTime
  val predictionEndTime: Time = this.predictionSimulator.finalTime

  private def buildPredictionSimulator: PedestrianPrediction = {

    val stateData: StateGroundTruth = this.getActualStateData

    val graph: GraphContainer = sim.graph.deepCopy(stateData.controlDevices)

    val predictionParameters: SimulationInputParameters = sim.getSetupArgumentsNew.deepCopy(graph, stateData.controlDevices)
    predictionParameters.trackDensityInterval = Some(densityUpdateInterval)

    predictionParameters.startTime = sim.currentTime
    predictionParameters.endTime = sim.currentTime + this.predictionHorizon
    predictionParameters.logFullPedestrianHistory = true

    val simulator: PedestrianPrediction = new PedestrianPrediction(predictionParameters)

    // Insertion of the population
    stateData.population.foreach(simulator.insertInPopulation)

    // clone the event list into the new simulator
    sim.cloneEventQueueInto(simulator)

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

    predictionSimulator = this.buildPredictionSimulator

    this.predictionSimulator.run()
  }

  def predict(decisionVariables: Iterable[ControlDevicePolicy]): Unit = {

    predictionSimulator = this.buildPredictionSimulator

    val amw: Map[String, Iterable[AMWPolicy]] = decisionVariables.collect{case amw: AMWPolicy => amw}.groupBy(_.name)

    this.predictionSimulator.controlDevices.amws.foreach(w => {
      w.setControlPolicy(amw(w.name))
      w.insertChangeSpeed(this.predictionSimulator)
    })

    this.predictionSimulator.run()

    /*new MovingPedestriansWithDensityWithWallVideo(
      "/home/nicholas/PhD/code/hub-simulator/tmp/moving_pedestrians_walls_" + this.predictionSimulator.ID + ".mp4",
      this.predictionSimulator.walls.filterNot(_.isInstanceOf[MovableWall]),
      math.max((1.0 / 0.1).toInt, 1),
      this.predictionSimulator.populationCompleted ++ this.predictionSimulator.population,
      this.predictionSimulator.criticalAreas.values,
      Map(),
      collection.mutable.ArrayBuffer(),
      scala.collection.mutable.ArrayBuffer(),
      (this.predictionSimulator.startTime.value to this.predictionSimulator.finalTime.value by 0.1).map(new Time(_)),
      this.predictionSimulator.controlDevices.flowSeparators
    )*/

  }

  def getPredictedStateData: StateGroundTruthPredicted = {

    new StateGroundTruthPredicted(
      this.predictionSimulator.startTime, this.predictionSimulator.finalTime, this.predictionInterval,
      (this.predictionSimulator.population ++ this.predictionSimulator.populationCompleted).toVector,
      this.predictionSimulator.controlDevices,
      this.predictionSimulator.criticalAreas
    )

  }

  def computeObjectives: Map[String, Double] = {

    val predictedData = this.getPredictedStateData

    val populationMvmtIdxs = predictedData.population
      .map(ped => (predictedData.intervals.indexWhere( _ > ped.entryTime), predictedData.intervals.indexWhere( _ > ped.exitTime) ))

    val inflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._1).view.mapValues(_.size).toMap
    val outflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._2).view.mapValues(_.size).toMap

    Map(
      "throughput" -> inflow.view.filterKeys(_ > 0).map(kv => kv._2 - outflow.getOrElse(kv._1, 0)).sum,
      "meanTT" -> predictedData.population.map(p => p.travelTime.value.toDouble).sum/predictedData.population.size
    )
  }

}





