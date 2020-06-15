package optimization.ALNS

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.control.amw.AMWPolicy
import hubmodel.prediction.{AMWFlowsFromGroundTruth, StatePrediction}
import hubmodel.prediction.state.StateGroundTruthPredicted

import scala.util.Random
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import tools.Time

import scala.annotation.tailrec


case class ALNSParameters(maxIterations: Int = 150,
                          weightScores: Map[String, Double] = Map("newBest" -> 20, "improvement" -> 15, "accepted" -> 10, "rejected" -> 1),
                          lambda: Double = 0.9,
                          initialScore: Double = 5.0,
                          maxScore: Double = 20,
                          minScore: Double = 0.5)

class ALNS(function: StatePrediction, initialPolicy: Iterable[ControlDevicePolicy], _operators: Vector[OperatorGenerator with RandomChange], constraints: Vector[Constraint], stochasticReduction: FunctionEvaluation => FunctionEvaluationReduced, parameters: ALNSParameters = new ALNSParameters) {

  // Reference to alns algorithm so that the inner classes can access the members and functions.
  alns: ALNS =>

  private class ExploreBestSolution extends Operator {

    def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
      alns.bestx._1
    }
  }

  private object ExploreBestSolution extends OperatorGenerator with RandomChange {
    val probability: Double = 0.075
    val name: String = "ExploreBestSolution"
    type T = ExploreBestSolution

    def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new ExploreBestSolution
  }

  private def stringify(x: Vector[ControlDevicePolicy]): String = x.sorted.map(dv => dv.nameToString + dv.decisionVariable.toString).mkString("-")

  private def computeObjective(x: Vector[ControlDevicePolicy]): Double = {
    stochasticReduction(evaluatedSolutions(stringify(x))._1)("density")
  }

  private def getOF(x: Vector[ControlDevicePolicy]): FunctionEvaluation = evaluatedSolutions(stringify(x))._1
  private def getStateData(x: Vector[ControlDevicePolicy]): Vector[StateGroundTruthPredicted] = evaluatedSolutions(stringify(x))._2


  private def changeSolution(x: Vector[ControlDevicePolicy], currentPredictedState: Vector[StateGroundTruthPredicted]): (Solution, String) = {

    val r: Double = ThreadLocalRandom.current.nextDouble()

    val weightSum: Double = this.operatorWeights.values.sum

    val op1Name = this.operatorWeights
      .toVector
      .map(w => w._2 / weightSum)
      .scanLeft(0.0)((a: Double, b: Double) =>  a + b)
      .zip(this.operatorWeights.toVector)
      .takeWhile(_._1 < r).last._2._1

    val op1 = operators.find(_.name == op1Name).get
    val tmp: Solution = op1.returnOperator(x, currentPredictedState).newSolution(x, function.getRealisedControlData._1)

    (tmp, op1.name)
  }

  @tailrec private def applyConstraints(cs: Vector[Constraint], currentSolution: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    if (cs.size == 1) {
      cs.head.checkFeasibility(currentSolution)
      cs.head.feasibleSolution
    }
    else {
      cs.head.checkFeasibility(currentSolution)
      applyConstraints(cs.tail, cs.head.feasibleSolution)
    }
  }

  private def acceptanceCriteriaSA(i: Int, x: Vector[ControlDevicePolicy]): (Boolean, Double) = {
    val T: Double = -0.5/math.log(0.99 + (0.00001-0.99)*i/this.maxIterations)
    if (computeObjective(x) < computeObjective(this.currentx._1)) {(true, T)}
    else {
      (math.exp((computeObjective(this.currentx._1) - computeObjective(x))/T) > ThreadLocalRandom.current.nextDouble(), T)
    }
  }


  private def isBestSolution(x: Solution): Boolean = {
    computeObjective(x._1) < computeObjective(this.bestx._1)
  }

  def optimalSolution: (Vector[ControlDevicePolicy], FunctionEvaluation, Vector[ControlDeviceData]) = (this.bestx._1, this.getOF(this.bestx._1), this.bestx._2)

  def optimize(): Unit = {
    var it: Int = 1
    while (it <= maxIterations && evaluatedSolutions(stringify(this.bestx._1))._2.size/function.replications < 0.3*maxIterations) {

      print("\r * Running simulation " + it + "/" + maxIterations)
      println("")
      val (xNewRaw, op): (Solution, String) = changeSolution(this.currentx._1, this.getStateData(this.currentx._1))
      val xNew: Solution = (applyConstraints(this.constraints, xNewRaw._1), xNewRaw._2)
      println("Evaluating solution: " + xNew._1.map(_.decisionVariable))
      function.predict(xNew._1, xNew._2)

      evaluatedSolutions.update(
        stringify(xNew._1),
        (
          (evaluatedSolutions.getOrElse(stringify(xNew._1), (Map(), Vector()))._1.toVector ++ function.computeObjectives.toVector).groupBy(_._1).view.mapValues(v => v.flatMap(_._2)).toMap,
          evaluatedSolutions.getOrElse(stringify(xNew._1), (Map(), Vector()))._2 ++ function.getPredictedStateData
        )
      )

      var accepted = false
      var score: Double = weightScores("rejected")
      val (accept, temp) = acceptanceCriteriaSA(it, xNew._1)

      if ( accept) {
        score = weightScores("accepted")
        if (computeObjective(xNew._1) < computeObjective(this.currentx._1)) { score = weightScores("improvement")}
        this.currentx = xNew
        accepted = true
      }

      if ( isBestSolution(xNew) ) {
        this.bestx = xNew
        score = weightScores("newBest")
      }

      operatorWeights.update(op, math.max(weightMin, math.min(weightMax, operatorWeights(op) * lambda  + (1-lambda) * score)))

      println(this.evaluatedSolutions(stringify(xNew._1))._2.size ,computeObjective(xNew._1), computeObjective(this.currentx._1), computeObjective(this.bestx._1))
      this.iterations.append((it, temp, xNew, accepted, op, stochasticReduction(this.getOF(xNew._1)), stochasticReduction(this.getOF(this.currentx._1)), stochasticReduction(this.getOF(this.bestx._1)), operatorWeights.toMap))

      it = it + 1
    }
    print("\n")
  }

  def writeIterationsToCSV(file: String, path: String = ""): Unit = {
    val objectiveHeader: Vector[String] = this.iterations.head._6.keys.toVector.sorted
    val header: Vector[String] = this.iterations.head._3._1.sorted.map(_.nameToString)
    val weightHeader: Vector[String] = this.iterations.head._9.keys.toVector

    this.iterations.toVector
      .map(v => Vector(v._1, v._2, v._5, v._4) ++ v._3._1.sorted.map(_.decisionVariable) ++ objectiveHeader.map(v._6) ++ objectiveHeader.map(v._7) ++ objectiveHeader.map(v._8) ++ weightHeader.map(v._9) )
      .transpose
      .writeToCSV(file, rowNames = None, columnNames = Some(Vector("it", "temp", "operator", "accepted") ++ header ++ objectiveHeader ++ objectiveHeader.map(_ ++ "_current") ++ objectiveHeader.map(_ ++ "_best") ++ weightHeader.map(_ ++ "_weight")))
  }

  println("Starting optimization for simulation")
  println(" * start time = " + this.function.predictionStartTime)
  println(" * end time = " + this.function.predictionEndTime)
  println(" * replications = " + this.function.replications)


  val weightMin: Double = parameters.minScore
  val weightMax: Double = parameters.maxScore
  val lambda: Double = parameters.lambda
  private val weightScores: Map[String, Double] = parameters.weightScores
  private val operators: Vector[OperatorGenerator with RandomChange] = this._operators :+ ExploreBestSolution
  private val operatorWeights: collection.mutable.TreeMap[String, Double] = collection.mutable.TreeMap.from(operators.map(o => o.name -> parameters.initialScore))

  private val x0: Vector[ControlDevicePolicy] = initialPolicy.toVector.sorted

  function.predict(x0, Vector())

  private val evaluatedSolutions: collection.mutable.Map[String, (FunctionEvaluation, Vector[StateGroundTruthPredicted])] = collection.mutable.Map(stringify(x0) -> (function.computeObjectives, function.getPredictedStateData))


  private var bestx: Solution = (x0, Vector())
  private var currentx: Solution = (x0, Vector())

  type OperatorWeights = Map[String, Double]

  private val iterations: collection.mutable.ArrayBuffer[(Iteration, Temperature, Solution, Boolean, OperatorName, FunctionEvaluationReduced, FunctionEvaluationReduced, FunctionEvaluationReduced, OperatorWeights)] =
    collection.mutable.ArrayBuffer((0, Double.NaN, bestx, true, "", stochasticReduction(getOF(x0)), stochasticReduction(getOF(x0)), stochasticReduction(getOF(x0)), operatorWeights.toMap))

  //val lowerBoundRandomFraction: Double = 0.2
  //val upperBoundRandomFraction: Double = 0.4
  val maxIterations: Int = parameters.maxIterations

  /*if (this.operators.collect{case rand: RandomChange => {rand.probability}}.sum != 1.0) {
    throw new Exception("Sum of probabilities for random operators different than one !")
  }*/

  //val sizeOfX: Int = x0.size
  //val orderedIdxOfX: Vector[Int] = (0 until sizeOfX).toVector

  /*val randomOperatorsProbCumSum: Vector[(Double, OperatorGenerator with RandomChange)] = {
    this.operators
      .collect{case rand: OperatorGenerator with RandomChange => {rand}}
      .scanLeft(0.0)((a: Double, b: OperatorGenerator with RandomChange) =>  a + b.probability)
      .zip(operators.collect{case rand: OperatorGenerator with RandomChange => {rand}})
  }*/
}

