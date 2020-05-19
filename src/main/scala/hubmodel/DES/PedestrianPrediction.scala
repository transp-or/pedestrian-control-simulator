package hubmodel.DES

class PedestrianPrediction(params: SimulationInputParameters) extends NOMADGraphSimulator(params) with IsPrediction {

  val isPrediction: Boolean = true

  override val verbose: Boolean = false

  override val simulationType: String = "prediction simulation"

  override def printSimulationInformation(): Unit = {}

  class StateEvaluation extends super.StateEval(this) with super.StateEvaluationActionDES

  class LogStatePrediction extends super.LogState() with super.LogStateDES

  insertStateEvaluationStart(new this.StateEvaluation)

}
