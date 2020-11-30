package optimization.ALNS

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.control.amw.AMWPolicy
import hubmodel.prediction.{AMWFlowsFromGroundTruth, StatePrediction}
import hubmodel.prediction.state.StateGroundTruthPredicted

import scala.util.Random
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import optimization.ALNS.constraints.Constraint
import optimization.ALNS.operators.{OperatorGenerator, RandomChange}
import tools.Time

import scala.annotation.tailrec


case class ALNSParameters(maxIterations: Int = 150,
                          weightScores: Map[String, Double] = Map("newBest" -> 20, "improvement" -> 15, "accepted" -> 10, "rejected" -> 1),
                          lambda: Double = 0.9,
                          initialScore: Double = 5.0,
                          maxScore: Double = 20,
                          minScore: Double = 0.5,
                          SATypicalIncrease: Double = 0.01)

class ALNSLinearCombination(f: StatePrediction,
                            xInit: Iterable[ControlDevicePolicy],
                            ops: Vector[OperatorGenerator with RandomChange],
                            cons: Vector[Constraint],
                            reduction: FunctionEvaluation => FunctionEvaluationReduced,
                            parameters: ALNSParameters = new ALNSParameters) extends MetaHeuristic (f, new Policy(xInit.toVector), ops, cons, reduction) {

  // Reference to alns algorithm so that the inner classes can access the members and functions.
  alns: ALNSLinearCombination =>

  /*private class ExploreBestSolution extends Operator {

    def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
      alns.bestx._1
    }
  }

  private object ExploreBestSolution extends OperatorGenerator with RandomChange {
    val name: String = "ExploreBestSolution"
    type T = ExploreBestSolution

    def returnOperator(x: Vector[ControlDevicePolicy], iterable: Vector[StateGroundTruthPredicted]): T = new ExploreBestSolution
  }*/

  @deprecated private def stringify(x: Policy): String = x.x.sorted.map(dv => dv.nameToString + dv.decisionVariable.toString).mkString("-")



  private def computeObjective(x: Policy): Double = {

    def normalizeQuantityMinMax(name: String): Double = {
        if (referenceValuesMax(name) > 0) {
          (stochasticReduction(evaluatedSolutions(x)._2)(name) - referenceValuesMin(name)) / (referenceValuesMax(name) - referenceValuesMin(name))
        } else {
          0.0
        }
    }

    def normalizeQuantityRef(name: String): Double = {
        if (referenceValues(name) > 0) {
          stochasticReduction(evaluatedSolutions(x)._2)(name) / referenceValues(name)
        } else {
          0.0
        }
    }

    def normalizeQuantityMaxUtopia(name: String): Double = {
      if (referenceValues(name) > 0) {
        (stochasticReduction(evaluatedSolutions(x)._2)(name) - utopiaValues(name)) / (referenceValues(name) - utopiaValues(name))
      } else {
        0.0
      }
    }

    normalizeQuantityMaxUtopia("linkTT") + normalizeQuantityMaxUtopia("density") + normalizeQuantityMaxUtopia("meanTT")
    //normalizeQuantityMaxUtopia("density") + normalizeQuantityMaxUtopia("meanTT")
  }

  protected def getOF(x: Policy): FunctionEvaluation = {
    evaluatedSolutions(x)._2
  }

  protected def getStateData(x: Policy): Vector[StateGroundTruthPredicted] = {
    evaluatedSolutions(x)._3
  }


  /*private def changeSolution(x: Vector[ControlDevicePolicy], currentPredictedState: Vector[StateGroundTruthPredicted]): (Solution, String) = {

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
      case _ => {    */op1.returnOperator(x, currentPredictedState).newSolution(x, function.getRealisedControlData._1)/* }
    }*/

    (tmp, op1.name)
  }*/

 /* @tailrec private def applyConstraints(cs: Vector[Constraint], currentSolution: Policy): Policy = {

    if (cs.size == 1) {
      cs.head.checkFeasibility(currentSolution)
      cs.head.feasibleSolution
    }
    else {
      cs.head.checkFeasibility(currentSolution)
      applyConstraints(cs.tail, cs.head.feasibleSolution)
    }
  }*/

  private def acceptanceCriteriaSA(i: Int, x: Policy): (Boolean, Double) = {
    val T: Double = -parameters.SATypicalIncrease/math.log(0.99 + (0.00001-0.99)*i/this.maxIterations)
    if (computeObjective(x) < computeObjective(this.currentx)) {
      (true, T)
    } else {
      (math.exp((computeObjective(this.currentx) - computeObjective(x))/T) > ThreadLocalRandom.current.nextDouble(), T)
    }
  }


  private def isBestSolution(x: Solution): Boolean = {
    computeObjective(x._1) < computeObjective(this.bestx)
  }

  def optimalSolution: (Policy, FunctionEvaluation, Vector[ControlDeviceData]) = (this.bestx, this.getOF(this.bestx), this.evaluatedSolutions(this.bestx)._1)

  def optimize(iterationFile: String, path: String = ""): Unit = {
    var it: Int = 1
    while (it <= maxIterations && evaluatedSolutions(this.bestx)._2.size/function.replications < 0.3*maxIterations) {
      println(" --------------------------------------------------------------------------------------------------------------------- ")
      print("\r * Running simulation " + it + "/" + maxIterations)
      println("")
      val (xNewRaw, op): (Solution, String) = changeSolution(this.currentx, this.getStateData(this.currentx), OperatorParameters())
      val xNew: Solution = (applyConstraints(this.constraints, xNewRaw._1), xNewRaw._2)

      println(" * current solution:")
      println(this.currentx.x.map(_.decisionVariable))
      println(" * new solution after operator: " + op)
      println(xNew._1.x.map(_.decisionVariable))



      function.predict(xNew._1.x, xNew._2)

      evaluatedSolutions.update(
        xNew._1,
        (xNew._2,
          (evaluatedSolutions.getOrElse(xNew._1, (xNew._2, Map(), Vector()))._2.toVector ++ function.computeObjectives.toVector).groupBy(_._1).view.mapValues(v => v.flatMap(_._2)).toMap,
          evaluatedSolutions.getOrElse(xNew._1, (xNew._2, Map(), Vector()))._3 ++ function.getPredictedStateData
        )
      )

      evaluatedSolutions.head._2._2.keys.foreach(k => {
        val objectiveFunctionReduced: FunctionEvaluationReduced = stochasticReduction(evaluatedSolutions(xNew._1)._2)
        referenceValuesMin.update(k, math.min(referenceValuesMin(k), objectiveFunctionReduced(k)))
        referenceValuesMax.update(k, math.max(referenceValuesMax(k), objectiveFunctionReduced(k)))
      })

      var score: Double = weightScores("rejected")
      val (accept, temp) = acceptanceCriteriaSA(it, xNew._1)

      // compute operator score if accepted
      if (accept) {
        score = weightScores("accepted")
      }

      // compute operator score if improvement
      if ( accept && computeObjective(xNew._1) <= computeObjective(this.currentx)) {
        score = weightScores("improvement")
      }

      // compute operator score if new best
      if ( accept && isBestSolution(xNew)) {
        score = weightScores("newBest")
      }

      // update current solution
      if ( accept) {
        this.updateCurrent(xNew._1)
      }


      val newBest = evaluatedSolutions.minBy(s => computeObjective(s._1))
      updateBestX(newBest._1)

      operatorWeights.update(op, math.max(weightMin, math.min(weightMax, operatorWeights(op) * lambda  + (1.0-lambda) * score)))

      val solutionReplications: Int = this.evaluatedSolutions(xNew._1)._3.size
      val OF: Double = computeObjective(xNew._1)
      val currentOF: Double = computeObjective(this.currentx)
      val bestOF: Double = computeObjective(this.bestx)
      println(s" * repl.: $solutionReplications, OF: $OF, current OF: $currentOF, best OF: $bestOF, operator score: $score" )

      this.iterations.append((it, temp, xNew._1, accept, op, computeObjective(xNew._1), stochasticReduction(this.getOF(xNew._1)), stochasticReduction(this.getOF(this.currentx)), stochasticReduction(this.getOF(this.bestx)), operatorWeights.toMap))

      it = it + 1
    }
    print("\n")
  }

  def writeIterationsToCSV(file: String, path: String = ""): Unit = {
    val objectiveHeader: Vector[String] = this.iterations.head._7.keys.toVector.sorted
    val header: Vector[String] = this.iterations.head._3.x.sorted.map(_.nameToString)
    val weightHeader: Vector[String] = this.iterations.head._10.keys.toVector

    this.iterations.toVector
      .map(v => Vector(v._1, v._2, v._5, v._4) ++ v._3.x.sorted.map(_.decisionVariable) ++ objectiveHeader.map(v._7) ++ objectiveHeader.map(v._8) ++ objectiveHeader.map(v._9) ++ weightHeader.map(v._10) )
      .transpose
      .writeToCSV(file, rowNames = None, columnNames = Some(Vector("it", "temp", "operator", "accepted") ++ header ++ objectiveHeader ++ objectiveHeader.map(_ ++ "_current") ++ objectiveHeader.map(_ ++ "_best") ++ weightHeader.map(_ ++ "_weight")))
  }

  def getPoints: Vector[(Int, Policy, FunctionEvaluationReduced)] = {
    this.iterations.map(it => (it._1, it._3, it._7)).toVector
  }


  /*  ------------------- ATTRIBUTES --------------------*/

  println("Starting optimization for simulation")
  println(" * start time = " + this.function.predictionStartTime)
  println(" * end time = " + this.function.predictionEndTime)
  println(" * replications = " + this.function.replications)

  val weightMin: Double = parameters.minScore
  val weightMax: Double = parameters.maxScore
  val lambda: Double = parameters.lambda
  /*private val weightScores: Map[String, Double] = parameters.weightScores
  private val operators: Vector[OperatorGenerator with RandomChange] = this._operators :+ ExploreBestSolution
  private val operatorWeights: collection.mutable.TreeMap[String, Double] = collection.mutable.TreeMap.from(operators.map(o => o.name -> parameters.initialScore))*/

  private val x0: Policy = initialPolicy

  function.predict(x0.x, Vector())

  private val evaluatedSolutions: collection.mutable.Map[Policy, (Vector[ControlDeviceData], FunctionEvaluation, Vector[StateGroundTruthPredicted])] = {
    collection.mutable.Map(x0 -> (Vector(), function.computeObjectives, function.getPredictedStateData))
  }

  private lazy val referenceValuesMin: collection.mutable.Map[String, Double] = collection.mutable.Map() ++ stochasticReduction(evaluatedSolutions((x0))._2)
  private lazy val referenceValuesMax: collection.mutable.Map[String, Double] = collection.mutable.Map() ++ stochasticReduction(evaluatedSolutions((x0))._2)
  private lazy val referenceValues: Map[String, Double] = stochasticReduction(evaluatedSolutions((x0))._2)
  private lazy val utopiaValues: Map[String, Double] = Map("density" -> 0.0, "meanTT" -> 0.85 * referenceValues("meanTT"), "linkTT" -> 0.0)

  //private var bestx: Solution = (x0, Vector())
  /*private var _bestX: Solution = (x0, Vector())
  private def bestx: Solution = this._bestX
  private def updateBestX(x: Solution): Unit = {this._bestX = x}*/

  //protected var currentx: Policy = x0

  //type OperatorWeights = Map[String, Double]


  //val lowerBoundRandomFraction: Double = 0.2
  //val upperBoundRandomFraction: Double = 0.4
  val maxIterations: Int = parameters.maxIterations
  val weightScores: Map[String, Double] = parameters.weightScores
  protected val operatorWeights: collection.mutable.TreeMap[String, Double] = collection.mutable.TreeMap.from(operators.map(o => o.name -> parameters.initialScore))


  private val iterations: collection.mutable.ArrayBuffer[(Iteration, Temperature, Policy, Boolean, OperatorName, Double, FunctionEvaluationReduced, FunctionEvaluationReduced, FunctionEvaluationReduced, OperatorWeights)] =
    collection.mutable.ArrayBuffer((0, Double.NaN, bestx, true, "", computeObjective(x0), stochasticReduction(getOF(x0)), stochasticReduction(getOF(x0)), stochasticReduction(getOF(x0)), operatorWeights.toMap))


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

