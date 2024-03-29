package hubmodel.DES

import hubmodel.control.amw.{AMWPolicy, MovingWalkwayControlEvents}
import hubmodel.control.{ControlDevicePolicy, EvaluateState, UpdateGates}
import hubmodel.io.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.prediction.{AMWFlowsFromGroundTruth, CongestionDataFromGroundTruth, PredictWithGroundTruth, StatePrediction}
import hubmodel.supply.continuous.MovableWall
import optimization.ALNS.{ALNSLinearCombination, ALNSParameters, ALNSPareto, FunctionEvaluation}
import tools.Time
import myscala.math.stats.{ComputeQuantiles, ComputeStats, computeQuantile}
import hubmodel.AMW_ACCELERATION_AMPLITUDE
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import optimization.ALNS.constraints.{SpeedLowerBound, SpeedUpperBound}
import optimization.ALNS.operators.{AccelerateAllSpeeds, DeccelerateAllSpeeds, DirectionMatchFlowCombinedSpeedUpdates, DownstreamDensityUpdate, RandomDecreaseSpeed, RandomIncreaseSpeed, RandomSetSpeed, RandomSetSpeedSpeedUpdates}

trait IsMainSimulation {

  /** container for keeping the prediction results */
  private var _prediction: Option[StatePrediction] = None

  def prediction: Option[StatePrediction] = this._prediction

  def updatePrediction(statePrediction: StatePrediction): Unit = {
    this._prediction = Some(statePrediction)
  }

  class StateEval(sim: PedestrianSimulation) extends EvaluateState(sim) with Action {

    override val priority: Int = 111

    override def execute(): Unit = {

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": state evaluation")

      //this.computeDensityAtCurrentTime()

      if (sim.useFlowGates || sim.useBinaryGates) {
        sim.insertEventWithZeroDelay(new UpdateGates(sim))
      }

      if (sim.useFlowSep && !sim.controlDevices.fixedFlowSeparators) {
        processIncomingFlowsForFS()
      }

      if (sim.controlDevices.amws.nonEmpty && this.sim.controlDevices.amwsMode._1 == "predictive") {
        // this is inserted elsewhere now.
        //sim.insertEventWithZeroDelay(new RollingHorizonOptimization(this.sim))
      } else if (sim.controlDevices.amws.nonEmpty && this.sim.controlDevices.amwsMode._1 == "reactive") {
        updateReactiveAMWs()
      }

      this.sim.insertEventWithDelay(sim.stateEvaluationInterval)(new StateEval(sim))
    }

    type A = StateEval

    //type B = PedestrianSimulation

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None

  }

  class LogState(sim: PedestrianSimulation) extends Action {

    override def execute(): Any = {
      sim.controlDevices.amws.foreach(w => w.appliedPolicy.append((sim.currentTime, w.speed(sim.currentTime))))
      sim.insertEventWithDelay(Time(0.5))(new LogState(this.sim))
    }
    type A = LogState

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }

  class RollingHorizonOptimization(sim: PedestrianSimulation) extends Action {

    override def execute(): Any = {

      val previousIntervalStart: Time = Time(math.max(sim.startTime.value.toDouble, (sim.currentTime - this.sim.predictionInputParameters.horizon).value.toDouble))

      println("writing main simulation from " + previousIntervalStart + " to " + sim.currentTime)
      /*new MovingPedestriansWithDensityWithWallVideo(
        sim.ID + "_" + previousIntervalStart.value.toString() + "_" + sim.currentTime +  ".mp4",
        sim.walls.filterNot(_.isInstanceOf[MovableWall]),
        math.max((1.0 / 0.1).toInt, 1),
        sim.populationCompleted ++ sim.population,
        sim.criticalAreas.values,
        Map(),
        collection.mutable.ArrayBuffer(),
        scala.collection.mutable.ArrayBuffer(),
        (previousIntervalStart.value to sim.currentTime.value by 0.1).map(new Time(_)),
        Vector()
      )*/

      val timeIntervals: Vector[Time] = sim.currentTime.value.until((sim.currentTime + this.sim.predictionInputParameters.horizon).value).by(this.sim.predictionInputParameters.decisionVariableLength.value).toVector.map(v => Time(v.toDouble))

      val tmp = sim.controlDevices.amws.flatMap(w => w.getControlPolicy)

      val initialControlPolicy: Iterable[ControlDevicePolicy] = (sim.controlDevices.amws
        .flatMap(w => timeIntervals.zip(Vector.fill(timeIntervals.size)((w.name, w.length))).map(t => (t._2, t._1)))
        //.filterNot(w => tmp.exists(p => p.name == w._1._1 && p.start == w._2))
        .map(t => AMWPolicy(t._1._1, t._2, t._2 + this.sim.predictionInputParameters.decisionVariableLength, 0.0, t._1._2))/* ++
        tmp*/).toVector.sorted



      def f(x: FunctionEvaluation): Map[String, Double] = x.view.mapValues(v => v.sum/v.size.toDouble).toMap


      val horizonOptimization: ALNSPareto = new ALNSPareto(
        new PredictWithGroundTruth(sim),
        initialControlPolicy,
        Vector(RandomIncreaseSpeed, RandomDecreaseSpeed, AccelerateAllSpeeds, DeccelerateAllSpeeds, DownstreamDensityUpdate, RandomSetSpeed, DirectionMatchFlowCombinedSpeedUpdates, RandomSetSpeedSpeedUpdates),
        Vector(SpeedUpperBound, SpeedLowerBound),
        f,
        sim.predictionInputParameters.ALNSParameters
      )

      horizonOptimization.optimize(sim.ID + "_" + sim.currentTime + "_" + (sim.currentTime + this.sim.predictionInputParameters.horizon) + "_" + this.sim.predictionInputParameters.decisionVariableLength, sim.outputDir)

      horizonOptimization.writeIterationsToCSV(sim.outputDir + "a_posteriori_points_" + sim.ID + "_" + sim.currentTime + "_" + (sim.currentTime + this.sim.predictionInputParameters.horizon) + "_" + this.sim.predictionInputParameters.decisionVariableLength + ".csv")



     /* (for (a <- horizonOptimization.getPoints; b <- horizonOptimization.getPoints if a._1 != b._1) yield {
        (
          math.pow(a._2.x.zip(b._2.x).map(t => math.pow((t._2.decisionVariable - t._1.decisionVariable), 2)).sum, 0.5),
          math.pow(math.pow(b._3("meanTT") - a._3("meanTT"), 2) + math.pow(b._3("density") - a._3("density"), 2), 0.5)
        )
      }).writeToCSV("distance-policy-OFs_" + sim.ID + "_" + sim.currentTime + "_" + (sim.currentTime + this.sim.predictionInputParameters.horizon) + "_" + this.sim.predictionInputParameters.decisionVariableLength + ".csv")
      */

      println(horizonOptimization.optimalSolution._1.x.sorted.map(_.decisionVariable))

      this.sim.controlDevices.amws
        .foreach(w => {
          val policy = horizonOptimization.optimalSolution._1.x.collect{case amw: AMWPolicy if amw.name == w.name => amw}
          w.expectedPolicy.append(policy.map(p => (p.start, p.speed)))
          val eventData: Option[MovingWalkwayControlEvents] = horizonOptimization.optimalSolution._3.collect{case data: MovingWalkwayControlEvents => data}.find(_.name == w.name)
          val additionalOpenTime = {
            if (w.getIsClosed &&  ((w.speed(sim.currentTime) > 0.0 && policy.head.speed < 0.0 ) ||(w.speed(sim.currentTime) < 0.0 && policy.head.speed > 0.0 ))) {
              Vector(sim.currentTime + Time(math.abs(w.speed(sim.currentTime))/AMW_ACCELERATION_AMPLITUDE))
            } else if (w.getIsClosed) {
              Vector(sim.currentTime)
            } else {
              Vector()
            }
          }
          w.setControlPolicy(policy, Some(eventData.getOrElse(MovingWalkwayControlEvents(w.name)).copy(openTime = (eventData.getOrElse(MovingWalkwayControlEvents(w.name)).openTime ++ additionalOpenTime).distinct)))
          w.insertChangeSpeed(this.sim)
        })


      sim.insertEventWithDelay(sim.predictionInputParameters.updateInterval)(new RollingHorizonOptimization(this.sim))

    }

    type A = RollingHorizonOptimization

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }

}
