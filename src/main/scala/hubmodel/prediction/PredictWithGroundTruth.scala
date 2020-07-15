package hubmodel.prediction

import hubmodel.DES.{PedestrianPrediction, PedestrianSimulation, SimulationInputParameters}
import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.control.amw.{AMWPolicy, MovingWalkway, MovingWalkwayControlEvents}
import hubmodel.io.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.ped.PedestrianNOMAD
import hubmodel.prediction.state.{StateGroundTruth, StateGroundTruthPredicted}
import hubmodel.supply.continuous.MovableWall
import hubmodel.supply.graph.GraphContainer
import hubmodel.{Position, writeEdgesCSV, writeEdgesJSON}
import tools.Time
import tools.TimeNumeric.mkOrderingOps

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.parallel.immutable.{ParSeq, ParVector}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import myscala.math.stats.{ComputeQuantiles, ComputeStats, computeQuantile}
import tools.cells.Vertex
import tools.math.integration.rectangleIntegration

import scala.util.{Failure, Success, Try}


/** Prediction using the ground truth simulator. This class makes a copy of the state of the simulation and then
  * performs a simulation to predict the upcoming state.
  *
  * @param sim
  */
class PredictWithGroundTruth(private val sim: PedestrianSimulation) extends StatePrediction {


  val predictionStartTime: Time = this.sim.currentTime
  val predictionEndTime: Time = this.sim.currentTime + this.sim.predictionInputParameters.horizon
  val predictionInterval: Time = this.sim.predictionInputParameters.updateInterval
  val replications: Int = sim.predictionInputParameters.replications

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
          predictionParameters.trackDensityInterval = Some(this.sim.predictionInputParameters.densityUpdateInterval)

          predictionParameters.startTime = sim.currentTime
          predictionParameters.endTime = sim.currentTime + this.sim.predictionInputParameters.horizon
          predictionParameters.logFullPedestrianHistory = true

          val simulator: PedestrianPrediction = new PedestrianPrediction(predictionParameters)

          // Insertion of the population
          stateData.population
            .foreach(p => {
              p._2.foreach(ar => p._1.appendAccomplishedRoute(ar._1, graph.vertexMapNew(ar._2), ar._3))
              simulator.insertInPopulation(p._1)
            })

          // clone the event list into the new simulator
          sim.cloneEventQueueInto(simulator)

          simulator
        })
    } else {
      Vector()
    }
  }

  private def buildPredictionSimulatorParallel: ParVector[PedestrianPrediction] = {

    if (sim.predictionInputParameters.threads > 1) {
      val parallelRuns: ParVector[Int] = Vector.range(0, sim.predictionInputParameters.replications).par
      parallelRuns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(math.min(sim.predictionInputParameters.replications, sim.predictionInputParameters.threads)))

      parallelRuns.map(i => {

        val stateData: StateGroundTruth = this.getActualStateData

        val graph: GraphContainer = sim.graph.deepCopy(stateData.controlDevices)

        val predictionParameters: SimulationInputParameters = sim.getSetupArgumentsNew.deepCopy(graph, stateData.controlDevices)
        predictionParameters.trackDensityInterval = Some(this.sim.predictionInputParameters.densityUpdateInterval)

        predictionParameters.startTime = sim.currentTime
        predictionParameters.endTime = sim.currentTime + this.sim.predictionInputParameters.horizon
        predictionParameters.logFullPedestrianHistory = true

        val simulator: PedestrianPrediction = new PedestrianPrediction(predictionParameters)

        // Insertion of the population
        stateData.population
          .foreach(p => {
            p._2.foreach(ar => p._1.appendAccomplishedRoute(ar._1, graph.vertexMapNew(ar._2), ar._3))
            simulator.insertInPopulation(p._1)
          })
        // clone the event list into the new simulator
        sim.cloneEventQueueInto(simulator)

        simulator
      })
    } else {
      ParVector()
    }
  }

  /** Collect the state data from the reference simulator which will be used as starting point for the prediction.
    *
    * @return
    */
  protected def getActualStateData: StateGroundTruth = {
    val pop: Vector[(PedestrianNOMAD, Vector[(Time, String, Position)])] = sim.population.map(p => p.copyState(sim.currentTime, true)).toVector
    val newDevices = sim.controlDevices.deepCopyWithState(sim.currentTime, pop.map(_._1))
    pop.foreach(p => p._1.updateBaseVelocity(newDevices.amws.toVector))
    new StateGroundTruth(pop, newDevices)
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

    // Calls the GC to clear memory
    System.gc()
  }

  def getRealisedControlData: Tuple1[Map[String, Double]] = {
    Tuple1(sim.controlDevices.amws.map(w => w.name -> w.speed(sim.currentTime)).toMap)
  }


  def predict(decisionVariables: Vector[ControlDevicePolicy], controlEvents: Vector[ControlDeviceData]): Unit = {

    predictionSimulatorSequential = this.buildPredictionSimulatorSequential
    predictionSimulatorParallel = this.buildPredictionSimulatorParallel
    val amw: Map[String, Iterable[AMWPolicy]] = decisionVariables.collect { case amw: AMWPolicy => amw }.groupBy(_.name)
    val eventData: Vector[MovingWalkwayControlEvents] = controlEvents.collect{ case amw: MovingWalkwayControlEvents => amw }

    this.predictionSimulatorSequential.foreach(s => {
      s.controlDevices.amws.foreach(w => {
        w.setControlPolicy(amw(w.name).toVector, eventData.find(_.name == w.name))
        w.insertChangeSpeed(s)
      })
    })

    this.predictionSimulatorParallel.foreach(s => {
      s.controlDevices.amws.foreach(w => {
        w.setControlPolicy(amw(w.name).toVector, eventData.find(_.name == w.name))
        w.insertChangeSpeed(s)
      })
    })

    this.predictionSimulatorSequential.foreach(s => {
      Try(s.run()) match {
        case Success(ok) => {
          /*new MovingPedestriansWithDensityWithWallVideo(
            "E:\\PhD\\hub-simulator\\SUCESS_moving_pedestrians_walls_" + s.ID + ".mp4",
            s.walls.filterNot(_.isInstanceOf[MovableWall]),
            math.max((1.0 / 0.1).toInt, 1),
            s.populationCompleted ++ s.population,
            s.criticalAreas.values,
            Map(),
            collection.mutable.ArrayBuffer(),
            scala.collection.mutable.ArrayBuffer(),
            (s.startTime.value to s.finalTime.value by 0.1).map(new Time(_)),
            s.controlDevices.flowSeparators
          )*/
        }
        case Failure(f) => {
          f.printStackTrace()
          println(f)
          new MovingPedestriansWithDensityWithWallVideo(
            "E:\\PhD\\hub-simulator\\ERROR_moving_pedestrians_walls_" + s.ID + ".mp4",
            s.walls.filterNot(_.isInstanceOf[MovableWall]),
            math.max((1.0 / 0.1).toInt, 1),
            s.populationCompleted ++ s.population,
            s.criticalAreas.values,
            Map(),
            collection.mutable.ArrayBuffer(),
            scala.collection.mutable.ArrayBuffer(),
            (s.startTime.value to s.finalTime.value by 0.1).map(new Time(_)),
            s.controlDevices.flowSeparators
          )
        }
      }
    })


    this.predictionSimulatorParallel.foreach(s => {
      Try(s.run()) match {
        case Success(ok) => {
          /*new MovingPedestriansWithDensityWithWallVideo(
            "E:\\PhD\\hub-simulator\\SUCESS_moving_pedestrians_walls_" + s.ID + ".mp4",
            s.walls.filterNot(_.isInstanceOf[MovableWall]),
            math.max((1.0 / 0.1).toInt, 1),
            s.populationCompleted ++ s.population,
            s.criticalAreas.values,
            Map(),
            collection.mutable.ArrayBuffer(),
            scala.collection.mutable.ArrayBuffer(),
            (s.startTime.value to s.finalTime.value by 0.1).map(new Time(_)),
            s.controlDevices.flowSeparators
          )*/
        }
        case Failure(f) => {
          f.printStackTrace()
          println(f)
          new MovingPedestriansWithDensityWithWallVideo(
            "E:\\PhD\\hub-simulator\\ERROR_moving_pedestrians_walls_" + s.ID + ".mp4",
            s.walls.filterNot(_.isInstanceOf[MovableWall]),
            math.max((1.0 / 0.1).toInt, 1),
            s.populationCompleted ++ s.population,
            s.criticalAreas.values,
            Map(),
            collection.mutable.ArrayBuffer(),
            scala.collection.mutable.ArrayBuffer(),
            (s.startTime.value to s.finalTime.value by 0.1).map(new Time(_)),
            s.controlDevices.flowSeparators
          )
        }
      }
    })



    /*predictionSimulatorSequential.foreach(sim => {
      //writeEdgesJSON(this.predictionSimulator.graph.edges, "edges_" + this.predictionSimulator.ID + ".json")
      writeEdgesCSV(sim.graph.edges, "edges_" + sim.ID + ".csv")
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
    })*/

  }

  def getPredictedStateData: Vector[StateGroundTruthPredicted] = {

    (this.predictionSimulatorSequential ++ this.predictionSimulatorParallel)
      .filter(s => s.exitCode != 0)
      .foreach(fail => println("Dropped simulation: " + fail.ID + "! exitCode=" + fail.exitCode))

    (this.predictionSimulatorSequential ++ this.predictionSimulatorParallel).collect {
      case s if s.exitCode == 0 => {
        val pop = (s.population ++ s.populationCompleted).toVector
        val intervals: Vector[Time] = s.startTime.value.to(s.finalTime.value).by(this.sim.predictionInputParameters.decisionVariableLength.value).map(v => Time(v.toDouble)).toVector
        val amwFlows = new AMWFlowsFromGroundTruthProcessor(s.controlDevices.amws.toVector, intervals).aggregateFlowsByAMW(pop)
        val densitiesInsideAreas: CongestionDataFromGroundTruth = new CongestionDataFromGroundTruth(s.criticalAreas, s.controlDevices.amws.collect{case w: MovingWalkway => w}.toVector, intervals)

        new StateGroundTruthPredicted(
          intervals,
          amwFlows,
          densitiesInsideAreas,
          {
            val populationMvmtIdxs = pop
              .map(ped => (intervals.indexWhere(_ > ped.entryTime), intervals.indexWhere(_ > ped.exitTime)))

            val inflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._1).view.mapValues(_.size).toMap
            val outflow: Map[Int, Int] = populationMvmtIdxs.groupBy(_._2).view.mapValues(_.size).toMap

            Map(
              "throughput" -> inflow.view.filterKeys(_ > 0).map(kv => kv._2 - outflow.getOrElse(kv._1, 0)).sum,
              "meanTT" -> pop.map(p => p.travelTime.value.toDouble).sum / pop.size,
              "density" -> s.criticalAreas.toVector.map(a => a._2.integratedIndividualDensity).sum
            )
          }
        )
      }
    }
  }

  def computeObjectives: Map[String, Vector[Double]] = {

    val predictedData = this.getPredictedStateData

    val data: Vector[Map[String, Double]] = predictedData.map(s => {

      s.indicators
    })

    val keys: Vector[String] = data.flatMap(_.keys).distinct

    keys.map(k => k -> /*computeQuantile(75)(*/ data.map(m => m(k)) /*.value*/).toMap
  }
}





