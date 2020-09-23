package optimization.ALNS

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.prediction.StatePrediction
import hubmodel.prediction.state.StateGroundTruthPredicted
import optimization.ALNS.constraints.Constraint
import optimization.ALNS.operators.{OperatorGenerator, RandomChange}

import scala.collection.mutable


class ALNSPareto(f: StatePrediction,
                 xInit: Iterable[ControlDevicePolicy],
                 ops: Vector[OperatorGenerator with RandomChange],
                 cons: Vector[Constraint],
                 reduction: FunctionEvaluation => FunctionEvaluationReduced,
                 parameters: ALNSParameters = new ALNSParameters
                ) extends MetaHeuristic(f, new Policy(xInit.toVector), ops, cons, reduction) with ParetoSet {


  override val stochasticReduction: FunctionEvaluation => FunctionEvaluationReduced = reduction

  protected val weightScores: Map[String, Temperature] = parameters.weightScores
  protected val operatorWeights: collection.mutable.TreeMap[String, Double] = collection.mutable.TreeMap.from(operators.map(o => o.name -> parameters.initialScore))
  protected val maxIterations: Iteration = parameters.maxIterations

  protected val weightMin: Double = parameters.minScore
  protected val weightMax: Double = parameters.maxScore
  protected val lambda: Double = parameters.lambda

  function.predict(xInit.toVector.sorted, Vector())
  this.insert(new Policy(xInit.toVector), Vector(), function.computeObjectives, function.getPredictedStateData)

  protected def getOF(x: Policy): FunctionEvaluation = {this.paretoSet(x)._2}

  protected def getStateData(x: Policy): Vector[StateGroundTruthPredicted] = {this.paretoSet(x)._3}

  def optimize(): Unit = {
    var it: Int = 1
    while (it <= maxIterations) {
      println(" --------------------------------------------------------------------------------------------------------------------- ")
      print("\r * Running simulation " + it + "/" + maxIterations)
      println("")
      val (xNewRaw, op): (Solution, String) = changeSolution(this.currentx, this.getStateData(this.currentx))
      val xNew: Solution = (applyConstraints(this.constraints, xNewRaw._1), xNewRaw._2)

      println(" * current solution:")
      println(this.currentx.x.map(_.decisionVariable))
      println(" * new solution after operator: " + op)
      println(xNew._1.x.map(_.decisionVariable))

      function.predict(xNew._1.x, xNew._2)

      val scoreText: String = this.insert(xNew._1, xNew._2, function.computeObjectives, function.getPredictedStateData)
      val score = weightScores(scoreText)

      // updates the next solution to use a source for generation
      this.updateCurrent(this.selectSolution)

      operatorWeights.update(op, math.max(weightMin, math.min(weightMax, operatorWeights(op) * lambda  + (1.0-lambda) * score)))

      if (scoreText == "accepted") {
      val solutionReplications: Int = this.paretoSet(xNew._1)._3.size
      stochasticReduction(this.paretoSet(xNew._1)._2).map(kv => kv._1 + ": " + kv._2.toString).mkString(", ")
      println(s" * repl.: $solutionReplications, operator score: $scoreText, objectives: " + stochasticReduction(this.paretoSet(xNew._1)._2).map(kv => kv._1 + ": " + kv._2.toString).mkString(", "))
      } else {
        println("rejected")
      }


      it = it + 1
    }
    print("\n")
  }

  def writeIterationsToCSV(file: String, path: String = ""): Unit = { }

  def optimalSolution: (Policy, FunctionEvaluation, Vector[ControlDeviceData]) = {
    val tmp = this.selectSolution
    this.updateBestX(tmp, this.paretoSet(tmp)._1)
    (this.bestx._1, this.getOF(this.bestx._1), this.bestx._2)
  }


}
