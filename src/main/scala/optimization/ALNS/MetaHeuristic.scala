package optimization.ALNS

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.ControlDevicePolicy
import hubmodel.prediction.StatePrediction
import hubmodel.prediction.state.StateGroundTruthPredicted
import optimization.ALNS.constraints.Constraint
import optimization.ALNS.operators.{Operator, OperatorGenerator, RandomChange}

import scala.annotation.tailrec

case class OperatorParameters(fraction: Option[Double] = None)

abstract class MetaHeuristic(val function: StatePrediction,
                             val initialPolicy: Policy,
                             _operators: Vector[OperatorGenerator with RandomChange],
                             val constraints: Vector[Constraint],
                             val stochasticReduction: FunctionEvaluation => FunctionEvaluationReduced) {
  metaheuristic =>

  type OperatorWeights = Map[String, Double]

  protected var _bestX: Policy = initialPolicy
  protected  def bestx: Policy = this._bestX
  protected  def updateBestX(x: Policy): Unit = {this._bestX = x}

  private class ExploreBestSolution extends Operator {

    def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
      metaheuristic.bestx.x
    }
  }

  private object ExploreBestSolution extends OperatorGenerator with RandomChange {
    val name: String = "ExploreBestSolution"
    type T = ExploreBestSolution

    def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = new ExploreBestSolution
  }

  protected def getOF(x: Policy): FunctionEvaluation

  protected def getStateData(x: Policy): Vector[StateGroundTruthPredicted]

  def updateCurrent(x: Policy): Unit = {this._currentx = x}
  def currentx: Policy = this._currentx
  private var _currentx: Policy = initialPolicy



  protected def changeSolution(x: Policy, currentPredictedState: Vector[StateGroundTruthPredicted], params: OperatorParameters): (Solution, String) = {

    val r: Double = ThreadLocalRandom.current.nextDouble()

    val weightSum: Double = this.operatorWeights.values.sum

    val op1Name = this.operatorWeights
      .toVector
      .map(w => w._2 / weightSum)
      .scanLeft(0.0)((a: Double, b: Double) =>  a + b)
      .zip(this.operatorWeights.toVector)
      .takeWhile(_._1 < r).last._2._1

    val op1 = operators.find(_.name == op1Name).get

    val tmp: Solution = /*op1Name match {
      case exploreBest if exploreBest == "ExploreBestSolution" => { op1.returnOperator(this.bestx._1, currentPredictedState).newSolution(x, function.getRealisedControlData._1) }
      case _ => {    */op1.returnOperator(x, currentPredictedState, params).newSolution(x, function.getRealisedControlData._1)/* }
    }*/

    (tmp, op1.name)
  }

  @tailrec final def applyConstraints(cs: Vector[Constraint], currentSolution: Policy): Policy = {

    if (cs.size == 1) {
      cs.head.checkFeasibility(currentSolution)
      cs.head.feasibleSolution
    }
    else {
      cs.head.checkFeasibility(currentSolution)
      applyConstraints(cs.tail, cs.head.feasibleSolution)
    }
  }

  protected val weightScores: Map[String, Double]// = parameters.weightScores

  protected  val operators: Vector[OperatorGenerator with RandomChange] = this._operators :+ ExploreBestSolution

  protected  val operatorWeights: collection.mutable.TreeMap[String, Double]// = collection.mutable.TreeMap.from(operators.map(o => o.name -> parameters.initialScore))

  protected val maxIterations: Int// = parameters.maxIterations

  protected val weightMin: Double// = parameters.minScore
  protected val weightMax: Double// = parameters.maxScore
  protected val lambda: Double// = parameters.lambda

  def optimize(iterationFile: String, path: String = ""): Unit

  def getPoints: Vector[(Int, Policy, FunctionEvaluationReduced)]

}
