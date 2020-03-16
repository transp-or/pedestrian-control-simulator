package optimization.ALNS

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import hubmodel.prediction.StatePrediction

import scala.util.Random

import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter


trait Operator {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy
}

trait RandomChange {
  val probability: Double
}

object RandomIncreaseSpeed extends Operator with RandomChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {AMWPolicy(amw.name, amw.start, amw.end, amw.speed + 1)}
    }
  }
  val probability: Double = 0.45
}

object RandomDecreaseSpeed extends Operator with RandomChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {AMWPolicy(amw.name, amw.start, amw.end, amw.speed - 1)}
    }
  }
  val probability: Double = 0.45
}

object RandomChangeDirection extends Operator with RandomChange {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = {
    x match {
      case amw: AMWPolicy => {AMWPolicy(amw.name, amw.start, amw.end, - amw.speed)}
    }
  }
  val probability: Double = 0.1
}

object CongestionIncreaseSpeed extends Operator {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = x
}

object CongestionDescreaseSpeed extends Operator {
  def xprime(x: ControlDevicePolicy): ControlDevicePolicy = x
}


trait Constraint {
  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean
  def makeFeasible: Vector[ControlDevicePolicy]
}

object SpeedUpperBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    DVSplit = DV.partition(dv => dv.decisionVariable <= 5.0)

    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {
    this.DVSplit._2.map(dv => dv match {
      case amw: AMWPolicy => amw.copy(speed = 5.0)
    }) ++ this.DVSplit._1
  }
}

object SpeedLowerBound extends Constraint {

  private var numberOfIterationsWithoutFeasiblity: Int = 0

  private var DVSplit: (Vector[ControlDevicePolicy], Vector[ControlDevicePolicy]) =  (Vector(), Vector())

  def checkFeasibility(DV: Vector[ControlDevicePolicy]): Boolean = {
    DVSplit = DV.partition(dv => dv.decisionVariable >= - 5.0)

    if (DVSplit._2.nonEmpty) {
      numberOfIterationsWithoutFeasiblity += 1
      false
    }
    else {true}
  }

  def makeFeasible: Vector[ControlDevicePolicy] = {
    this.DVSplit._2.map(dv => dv match {
      case amw: AMWPolicy => amw.copy(speed = -5.0)
    }) ++ this.DVSplit._1
  }
}

class ALNS(function: StatePrediction, initialPolicy: Iterable[ControlDevicePolicy], operators: Vector[Operator], constraints: Vector[Constraint]) {

  println("Starting optimization for simulation")
  println(" * start time = " + this.function.predictionStartTime)
  println(" * end time = " + this.function.predictionEndTime)
  println(" * decision variables interval = " + this.function.predictionInterval)


  private val x0: Vector[ControlDevicePolicy] = initialPolicy.toVector

  function.predict(x0)
  function.getPredictedStateData

  private var currentBestx: Vector[ControlDevicePolicy] = x0

  private var currentBestOF: FunctionEvaluation = function.computeObjectives

  def optimalSolution: (Vector[ControlDevicePolicy], FunctionEvaluation) = (this.currentBestx, this.currentBestOF)

  private val solutions: collection.mutable.ArrayBuffer[(Vector[ControlDevicePolicy], FunctionEvaluation)] = collection.mutable.ArrayBuffer((currentBestx, currentBestOF))

  val lowerBoundRandomFraction: Double = 0.25
  val upperBoundRandomFraction: Double = 0.75
  val maxIterations: Int = 50

  if (operators.collect{case rand: RandomChange => {rand.probability}}.sum != 1.0) {
    throw new Exception("Sum of probabilities for random operators different than one.")
  }

  val sizeOfX: Int = x0.size
  val orderedIdxOfX: Vector[Int] = (0 until sizeOfX).toVector

  val randomOperatorsProbCumSum: Vector[(Double, Operator with RandomChange)] = {
    operators
      .collect{case rand: Operator with RandomChange => {rand}}
      .scanLeft(0.0)((a: Double, b: Operator with RandomChange) =>  a + b.probability)
      .zip(operators.collect{case rand: Operator with RandomChange => {rand}})
  }

  def selectRandomChange: Vector[(ControlDevicePolicy, Operator with RandomChange)] = {
    val fractionToChange: Double = ThreadLocalRandom.current.nextDouble(lowerBoundRandomFraction, upperBoundRandomFraction)
    Random.shuffle(orderedIdxOfX)
      .take((fractionToChange * sizeOfX).round.toInt)
      .map(idx => {
        val operatorSample: Double = ThreadLocalRandom.current.nextDouble()
        (x0(idx), randomOperatorsProbCumSum.takeWhile(_._1 < operatorSample).last._2)
      })
  }

  def changeSolution(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val randomChanges = selectRandomChange

    x.map(dv => {
      val change: Option[(ControlDevicePolicy, Operator with RandomChange)] = randomChanges.find(rc => rc._1.start == dv.start)
      if (change.isDefined) {change.get._2.xprime(dv) }
      else {dv}
    })
  }

  def applyConstraints(cs: Vector[Constraint], currentSolution: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    if (cs.size == 1) {
      cs.head.checkFeasibility(currentSolution)
      cs.head.makeFeasible
    }
    else {
      cs.head.checkFeasibility(currentSolution)
      applyConstraints(cs.tail, cs.head.makeFeasible)
    }
  }

  def optimize(): Unit = {
    var it: Int = 0
    while (it < maxIterations) {

      print("\r * Running simulation " + it + "/" + maxIterations)
      //System.out.print(")
      val xNewRaw = changeSolution(this.currentBestx)
      val xNew = applyConstraints(this.constraints, xNewRaw)
      //println(xNew)
      function.predict(xNew)
      val fNew = function.computeObjectives
      //println(fNew)
      this.solutions.append((xNew, fNew))
      if ( fNew("meanTT") <=  this.currentBestOF("meanTT")) {
        this.currentBestOF = fNew
        this.currentBestx = xNew
        //this.solutions.sortBy(_._2("meanTT")).take(10).foreach(d => println(d._2("meanTT"), d._1.sortBy(_.start).map(v => v.asInstanceOf[AMWPolicy]).map(_.speed)))
      }
      it = it + 1
    }
    print("\n")
  }

  def writeSolutionToCSV(file: String, path: String = ""): Unit = {
    val objectiveHeader: Vector[String] = this.solutions.head._2.keys.toVector.sorted
    val header: Vector[String] = this.solutions.head._1.sortBy(_.start).map(_.nameToString)

    this.solutions.toVector
      .map(v => objectiveHeader.map(v._2) ++ v._1.sortBy(_.start).map(_.decisionVariable))
      .transpose
      .writeToCSV(file, rowNames = None, columnNames = Some(objectiveHeader ++ header))
  }

}
