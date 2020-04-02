package hubmodel.DES

import hubmodel.control.amw.AMWPolicy
import hubmodel.control.{EvaluateState, UpdateGates}
import hubmodel.prediction.{AMWFlowsFromGroundTruth, CongestionDataFromGroundTruth, PredictWithGroundTruth, StatePrediction}
import optimization.ALNS.{ALNS, CongestionDescreaseSpeed, CongestionIncreaseSpeed, MinimumDurationSameDirection, RandomChangeDirection, RandomDecreaseSpeed, RandomIncreaseSpeed, SpeedLowerBound, SpeedUpperBound}
import tools.Time

trait IsMainSimulation {

  val predictionHorizon: Time = Time(60)
  val predictionUpdateInterval: Time = Time(30)
  val predictionInterval: Time = Time(10)
  val densityUpdateInterval: Time = Time(5)



  /** container for keeping the prediction results */
  private var _prediction: Option[StatePrediction] = None

  def prediction: Option[StatePrediction] = this._prediction

  def updatePrediction(statePrediction: StatePrediction): Unit = {
    this._prediction = Some(statePrediction)
  }

  class StateEval(sim: PedestrianSimulation) extends EvaluateState(sim) with Action {

    override def execute(): Unit = {

      sim.eventLogger.trace("sim-time=" + sim.currentTime + ": state evaluation")

      this.computeDensityAtCurrentTime()

      if (sim.useFlowGates || sim.useBinaryGates) {
        sim.insertEventWithZeroDelay(new UpdateGates(sim))
      }

      if (sim.useFlowSep && !sim.controlDevices.fixedFlowSeparators) {
        processIncomingFlowsForFS()
      }

      sim.insertEventWithZeroDelay(new RollingHorizonOptimization(this.sim))

      this.sim.insertEventWithDelay(sim.stateEvaluationInterval)(new StateEval(sim))
    }

    type A = StateEval

    //type B = PedestrianSimulation

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None

  }

  class RollingHorizonOptimization(sim: PedestrianSimulation) extends Action {


    override def execute(): Any = {

      val timeIntervals: Vector[Time] = sim.currentTime.value.until((sim.currentTime + predictionHorizon).value).by(predictionInterval.value).toVector.map(v => Time(v.toDouble))

      val initialControlPolicy = sim.controlDevices.amws
        .flatMap(w => timeIntervals.zip(Vector.fill(timeIntervals.size)(w.name)).map(t => (t._2, t._1)))
        .map(t => AMWPolicy(t._1, t._2, t._2 + predictionInterval, 0.0))

      val horizonOptimization: ALNS = new ALNS(
        new PredictWithGroundTruth(sim, predictionHorizon, predictionInterval, densityUpdateInterval),
        initialControlPolicy,
        Vector(RandomIncreaseSpeed, RandomDecreaseSpeed, RandomChangeDirection, MinimumDurationSameDirection),
        Vector(SpeedUpperBound, SpeedLowerBound))

      horizonOptimization.optimize()

      horizonOptimization.writeSolutionToCSV("NS_points_" + sim.ID + "_" + sim.currentTime + "_" + (sim.currentTime + predictionHorizon) + "_" + predictionInterval + ".csv" ,"/home/nicholas/PhD/code/hub-simulator/")


      println(horizonOptimization.optimalSolution)

      /*val flows: Option[AMWFlowsFromGroundTruth] = sim.prediction.map(pred => {
        new CongestionDataFromGroundTruth(pred.getPredictedStateData.criticalAreas, timeIntervals)

        new AMWFlowsFromGroundTruth(
          pred.getPredictedStateData.population,
          pred.getPredictedStateData.controlDevices.amws.toVector,
          timeIntervals
        )
      })*/

      //val prediction = new PredictWithGroundTruth(sim, predictionHorizon, predictionInterval, densityUpdateInterval)


      this.sim.controlDevices.amws
        .foreach(w => {
          val policy = horizonOptimization.optimalSolution._1.collect{
            case amw: AMWPolicy if amw.name == w.name => amw
          }
          w.setControlPolicy(policy)
          w.insertChangeSpeed(this.sim)
        })

    }



    type A = RollingHorizonOptimization

    override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
  }

}
