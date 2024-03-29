package hubmodel.DES

import hubmodel.Position
import hubmodel.demand.CreatePedestrian
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.graph.MyEdge

import tools.Time
import tools.TimeNumeric.mkOrderingOps

import tools.cells.Vertex

import java.util.concurrent.ThreadLocalRandom
import scala.util.Random

class PedestrianPrediction(params: SimulationInputParameters) extends NOMADGraphSimulator(params) with IsPrediction {

  val insertErrors: Vector[SimulationErrors] = {
    {
      params.predictionParameters.predictionRandomError match {
        case Some(random) => {
          Vector(PredictionDemandRandomError(random, this.ODZones.toVector))
        }
        case None => {
          Vector()
        }
      }
    } ++ {
      params.predictionParameters.predictionScaleError match {
        case Some(scale) => {
          Vector(PredictionDemandScaleError(scale))
        }
        case None => {
          Vector()
        }
      }
    }
  }

  val isPrediction: Boolean = true

  override val verbose: Boolean = false

  override val simulationType: String = "prediction simulation"

  override def printSimulationInformation(): Unit = {}

  class StateEvaluation extends super.StateEval(this) with super.StateEvaluationActionDES

  class LogStatePrediction extends super.LogState() with super.LogStateDES

  insertStateEvaluationStart(new this.StateEvaluation)

}
