package hubmodel.prediction

import hubmodel.DES.{PedestrianPrediction, PedestrianSimulation, SimulationInputParameters}
import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import hubmodel.io.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.prediction.state.{StateGroundTruth, StateGroundTruthPredicted}
import hubmodel.supply.continuous.MovableWall
import hubmodel.supply.graph.GraphContainer
import hubmodel.{writeEdgesCSV, writeEdgesJSON}
import tools.Time
import tools.TimeNumeric.mkOrderingOps

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.parallel.immutable.{ParSeq, ParVector}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport

import myscala.math.stats.{ComputeQuantiles, ComputeStats}


/** Prediction using the ground truth simulator. This class makes a copy of the state of the simulation and then
  * performs a simulation to predict the upcoming state.
  *
  * @param sim
  */
class PredictWithGroundTruth(private val sim: PedestrianSimulation, predictionHorizon: Time, val predictionInterval: Time, densityUpdateInterval: Time) extends StatePrediction {

  val predictionStartTime: Time = this.sim.currentTime
  val predictionEndTime: Time = this.sim.currentTime + this.sim.predictionInputParameters.horizon

  private var predictionSimulatorSequential: Vector[PedestrianPrediction] = this.buildPredictionSimulatorSequential
  private var predictionSimulatorParallel: ParVector[PedestrianPrediction] = this.buildPredictionSimulatorParallel

  private def buildPredictionSimulatorSequential: Vector[PedestrianPrediction] = {

    if (sim.predictionInputParameters.threads == 1) {
      Vector
        .range(0, sim.predictionInputParameters.replications)
        .map(i => {
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
        })
    } else { Vector()}
  }

  private def buildPredictionSimulatorParallel: ParVector[PedestrianPrediction] = {

    if (sim.predictionInputParameters.threads > 1) {
      val parallelRuns: ParVector[Int] = Vector.range(0, sim.predictionInputParameters.replications).par
      parallelRuns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(math.min(sim.predictionInputParameters.replications, sim.predictionInputParameters.threads)))

      parallelRuns.map(i => {

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
      })
    } else {ParVector()}
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

  predictionSimulatorSequential = this.buildPredictionSimulatorSequential
    predictionSimulatorParallel = this.buildPredictionSimulatorParallel

    if (this.predictionSimulatorSequential.nonEmpty) {
      this.predictionSimulatorSequential.foreach(_.run())
    }

    if (this.predictionSimulatorParallel.nonEmpty) {
      this.predictionSimulatorParallel.foreach(_.run())
    }
  }

  def predict(decisionVariables: Iterable[ControlDevicePolicy]): Unit = {

    predictionSimulatorSequential = this.buildPredictionSimulatorSequential
    predictionSimulatorParallel = this.buildPredictionSimulatorParallel
    val amw: Map[String, Iterable[AMWPolicy]] = decisionVariables.collect{case amw: AMWPolicy => amw}.groupBy(_.name)

    this.predictionSimulatorSequential.foreach(s => {
      s.controlDevices.amws.foreach(w => {
        w.setControlPolicy(amw(w.name))
        w.insertChangeSpeed(s)
      })
    })

    this.predictionSimulatorParallel.foreach(s => {
      s.controlDevices.amws.foreach(w => {
        w.setControlPolicy(amw(w.name))
        w.insertChangeSpeed(s)
      })
    })

    this.predictionSimulatorSequential.foreach(_.run())
    this.predictionSimulatorParallel.foreach(_.run())

    //writeEdgesJSON(this.predictionSimulator.graph.edges, "edges_" + this.predictionSimulator.ID + ".json")
    //writeEdgesCSV(this.predictionSimulator.graph.edges, "edges_" + this.predictionSimulator.ID + ".csv")

    predictionSimulatorParallel.foreach(sim => {
    new MovingPedestriansWithDensityWithWallVideo(
      "E:\\PhD\\hub-simulator\\moving_pedestrians_walls_" + sim.ID + ".mp4",
      sim.walls.filterNot(_.isInstanceOf[MovableWall]),
      math.max((1.0 / 0.1).toInt, 1),
      sim.populationCompleted ++ sim.population,
      sim.criticalAreas.values,
      Map(),
      collection.mutable.ArrayBuffer(),
      scala.collection.mutable.ArrayBuffer(),
      (sim.startTime.value to sim.finalTime.value by 0.1).map(new Time(_)),
      sim.controlDevices.flowSeparators
    )
    })

  }

  def getPredictedStateData: Vector[StateGroundTruthPredicted] = {

    (this.predictionSimulatorSequential ++ this.predictionSimulatorParallel).map(s => new StateGroundTruthPredicted(
      s.startTime, s.finalTime, this.predictionInterval,
      (s.population ++ s.populationCompleted).toVector,
      s.controlDevices,
      s.criticalAreas
    )
    )

  }

  def computeObjectives: Map[String, Double]  = {

    val predictedData = this.getPredictedStateData

    val data: Vector[Map[String, Double]] = predictedData.map(s => {

      val populationMvmtIdxs = s.population
        .map(ped => (s.intervals.indexWhere( _ > ped.entryTime), s.intervals.indexWhere( _ > ped.exitTime) ))

      val inflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._1).view.mapValues(_.size).toMap
      val outflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._2).view.mapValues(_.size).toMap


        Map(
          "throughput" -> inflow.view.filterKeys(_ > 0).map(kv => kv._2 - outflow.getOrElse(kv._1, 0)).sum,
          "meanTT" -> s.population.map(p => p.travelTime.value.toDouble).sum/s.population.size
        )
    })

    val keys: Vector[String] = data.flatMap(_.keys).distinct.toVector

    keys.map(k => k -> data.map(m => m(k)).toVector.statistics.median).toMap

  }
}





