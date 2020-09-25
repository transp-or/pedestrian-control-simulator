package optimization.ALNS.operators

import java.util.concurrent.ThreadLocalRandom

import hubmodel.control.ControlDevicePolicy
import hubmodel.control.amw.AMWPolicy
import hubmodel.prediction.state.StateGroundTruthPredicted
import myscala.math.stats.ComputeStats
import optimization.ALNS._
import tools.Time
import tools.TimeNumeric.mkOrderingOps

import scala.collection.MapView
import scala.util.Random


trait OperatorGenerator {

  val name: String

  type T <: Operator

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T
}

trait Operator {

  protected def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy]

  def newSolution(x: Policy, controlData: Map[String, Double]): Solution = {
    enforceSpeedChangeIntoPolicy(this.xprime(x.x), controlData)
  }

}

trait RandomChange

/** Selected between 20% and 100% of all of the control policies and increase the speed by SPEED_INCREMENT.
  *
  */
class RandomIncreaseSpeed(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = (fraction.getOrElse(ThreadLocalRandom.current().nextDouble(0.2,0.8)) * x.size).round.toInt
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amw: AMWPolicy, idx: Int) if idxToChange.contains(idx) => {
        amw.copy(speed = amw.speed + SPEED_INCREMENT)
      }
      case (a: ControlDevicePolicy, idx: Int) => a
    }

    tmp
  }
}

object RandomIncreaseSpeed extends OperatorGenerator with RandomChange {
  val name: String = "IncreaseSpeed"
  type T = RandomIncreaseSpeed

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new RandomIncreaseSpeed(params.fraction)
  }
}

/** Selected between 20% and 100% of all of the control policies and decreases the speed by SPEED_INCREMENT.
  *
  */
class RandomDecreaseSpeed(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = (fraction.getOrElse(ThreadLocalRandom.current().nextDouble(0.2,0.8)) * x.size).round.toInt
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amw: AMWPolicy, idx: Int) if idxToChange.contains(idx) => {
        amw.copy(speed = amw.speed - SPEED_INCREMENT)
      }
      case (a: ControlDevicePolicy, idx: Int) => a
    }

    tmp
  }
}

object RandomDecreaseSpeed extends OperatorGenerator with RandomChange {
  val name: String = "DecreaseSpeed"

  type T = RandomDecreaseSpeed

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T =  {
    new RandomDecreaseSpeed(params.fraction)
  }
}


/** Select between 20% and 100% of the policies and change the direction of the moving walkways for the selected
  * intervals. The speed magnitude is kept.
  */
class RandomChangeDirection(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = (fraction.getOrElse(ThreadLocalRandom.current().nextDouble(0.2,0.8)) * x.size).round.toInt
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amw: AMWPolicy, idx: Int) if idxToChange.contains(idx) => {
        amw.copy(speed = -amw.speed)
      }
      case (a: ControlDevicePolicy, _) => a
    }

    tmp
  }
}


object RandomChangeDirection extends OperatorGenerator with RandomChange {
  val name: String = "ChangeOneDirection"

  type T = RandomChangeDirection

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new RandomChangeDirection(params.fraction)
  }
}

/** Increase the magnitude of all AMWs. Basically positive speeds are increased by SPEED_INCREMENT and negative speeds
  * are decreased by SPEED_INCREMENT.
  *
  */
class AccelerateAllSpeeds(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = (fraction.getOrElse(ThreadLocalRandom.current().nextDouble(0.2,0.8)) * x.size).round.toInt
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amwP: AMWPolicy, idx: Int) if (amwP.speed.sign >= 0.0 && idxToChange.contains(idx)) => {
        amwP.copy(speed = amwP.speed + SPEED_INCREMENT)
      }
      case (amwP: AMWPolicy, idx: Int) if (amwP.speed.sign < 0.0 && idxToChange.contains(idx)) => {
        amwP.copy(speed = amwP.speed - SPEED_INCREMENT)
      }
      case (a: ControlDevicePolicy, idx: Int) => a
    }

    tmp
  }
}

object AccelerateAllSpeeds extends OperatorGenerator with RandomChange {
  val name: String = "AccelerateAllSpeeds"

  type T = AccelerateAllSpeeds

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new AccelerateAllSpeeds(params.fraction)
  }
}


/** Decrease the magnitude of all AMWs. Basically positive speeds are decreased by SPEED_INCREMENT and negative speeds
  * are increased by SPEED_INCREMENT.
  *
  */
class DeccelerateAllSpeeds(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val fractionToChange: Int = (fraction.getOrElse(ThreadLocalRandom.current().nextDouble(0.2,0.8)) * x.size).round.toInt
    val idxToChange: Vector[Int] = Random.shuffle(x.indices.toVector).take(fractionToChange)

    val tmp = x.zipWithIndex.map {
      case (amwP: AMWPolicy, idx: Int) if (amwP.speed.sign >= 0.0 && idxToChange.contains(idx)) => {
        amwP.copy(speed = amwP.speed - SPEED_INCREMENT)
      }
      case (amwP: AMWPolicy, idx: Int) if (amwP.speed.sign < 0.0 && idxToChange.contains(idx)) => {
        amwP.copy(speed = amwP.speed + SPEED_INCREMENT)
      }
      case (a: ControlDevicePolicy, idx: Int) => a
    }

    tmp
  }
}

object DeccelerateAllSpeeds extends OperatorGenerator with RandomChange {
  val name: String = "DeccelerateAllSpeeds"

  type T = DeccelerateAllSpeeds

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new DeccelerateAllSpeeds(params.fraction)
  }
}

/** Sets the direction to match the pedestrian flow measured parallel to AMWs. The speed magnitude is set to the
  * maximum possible speed. Each moving walkway has a 60% chance of being selected
  *
  * This operator should be combined with one of the speed increase and/or decrease operators. Otherwise this operator
  * will yield very similar results.
  *
  * @param flowDataBySimulation
  * @param timeIntervals
  */
class DirectionMatchFlow(val flowDataBySimulation: Vector[Map[(String, Int, Int), Double]], val timeIntervals: Vector[Time], fraction: Option[Double]) extends Operator {

  val flowData: Map[(String, Int, Int), Double] = flowDataBySimulation.flatMap(_.toVector).groupBy(_._1).view.mapValues(v => v.map(_._2).statistics.median).toMap

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val amwToChange: Vector[String] = x.map(_.name).distinct.filter(n => ThreadLocalRandom.current().nextDouble() < fraction.getOrElse(0.5))

    val tmp = x.map {
      case amw: AMWPolicy => {
        val flow = flowData.filter(v => v._1._1 == amw.name && timeIntervals(v._1._3) == amw.start).maxByOption(_._2)
        if (amwToChange.contains(amw.name) && flow.exists(f => f._1._2.sign != amw.speed.sign)) {
          amw.copy(speed = flow.get._1._2.sign * MAXIMUM_SPEED)
        }
        else {
          amw
        }
      }
      case other => other
    }
    tmp
  }
}

object DirectionMatchFlow extends OperatorGenerator with RandomChange {
  val name: String = "DirectionMatchFlow"

  type T = DirectionMatchFlow

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new DirectionMatchFlow(iterable.map(_.amwFlows.aggregateFlowsByAMW), iterable.head.intervals, params.fraction)
  }
}

/** Combined match flow with the speed decrease and increase.
  *
  * @param flowDataBySimulation
  * @param timeIntervals
  */
class DirectionMatchFlowCombinedSpeedUpdates(val flowDataBySimulation: Vector[Map[(String, Int, Int), Double]], val timeIntervals: Vector[Time], fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val matchFlow: DirectionMatchFlow = new DirectionMatchFlow(flowDataBySimulation, timeIntervals, None)

    val increaseSpeed: RandomIncreaseSpeed = new RandomIncreaseSpeed(fraction)
    val decreaseSpeed: RandomDecreaseSpeed = new RandomDecreaseSpeed(fraction)

    increaseSpeed.xprime(decreaseSpeed.xprime(matchFlow.xprime(x)))

  }
}

object DirectionMatchFlowCombinedSpeedUpdates extends OperatorGenerator with RandomChange {
  val name: String = "DirectionMatchFlowCombinedSpeedUpdates"

  type T = DirectionMatchFlowCombinedSpeedUpdates

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new DirectionMatchFlowCombinedSpeedUpdates(iterable.map(_.amwFlows.aggregateFlowsByAMW), iterable.head.intervals, params.fraction)
  }
}

class MinimumDurationSameDirection(allPolicy: Vector[ControlDevicePolicy]) extends Operator {

  val (amwPolicies: Map[String, Vector[AMWPolicy]], other: Vector[ControlDevicePolicy]) = groupAMWPolicies(allPolicy)

  val directionChangesIdx: MapView[String, Vector[Int]] = amwPolicies.view.mapValues(policy => policy
    .sliding(2)
    .zipWithIndex
    .collect { case change if change._1.head.speed.sign != change._1.last.speed.sign => change._2 + 1 }
    .toVector)

  val blockChangeIdx: Map[String, Vector[Int]] = amwPolicies
    .map(kv => (kv._1 -> Vector(0).appendedAll(directionChangesIdx(kv._1)).appended(amwPolicies(kv._1).size)))

  val blockProbabilities: Map[String, (Vector[Double], Int)] = blockChangeIdx.view
    .mapValues(policy => {
      val blockLengths = policy.sliding(2).map(pair => pair.last - pair.head).toVector
      val tmp: Double = blockLengths.map(_ / policy.size.toDouble).map(math.pow(_, -1.0)).sum
      val probs = blockLengths
        .map(_ / policy.size.toDouble)
        .map(math.pow(_, -1.0) / tmp)
        .scanLeft(0.0)((a: Double, b: Double) => a + b)
      val blockToChange = probs.indexWhere(_ > ThreadLocalRandom.current().nextDouble()) - 1
      (probs, blockToChange)
    }).toMap

  // Used when all same direction and randomly change block direction
  val noDirChangeData: Map[String, (Int, Boolean)] = amwPolicies.view.mapValues(policy => {
    val orderedIdxOfX: Vector[Int] = policy.indices.toVector
    val idxToChange: Int = Random.shuffle(orderedIdxOfX.dropRight(1)).head
    val changeFirstBlock: Boolean = ThreadLocalRandom.current().nextBoolean()
    (idxToChange, changeFirstBlock)
  }).toMap


  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    val tmp = x.map {
      case amw: AMWPolicy => {
        if (blockChangeIdx(amw.name).size > 2) {
          val idx: Int = amwPolicies(amw.name).indexWhere(w => amw.start == w.start && amw.end == w.end && amw.name == w.name)
          if (blockChangeIdx(amw.name)(blockProbabilities(amw.name)._2) <= idx && idx < blockChangeIdx(amw.name)(blockProbabilities(amw.name)._2 + 1)) {
            amw.copy(speed = -amw.speed)
          } else {
            amw
          }
        }
        else if (blockChangeIdx(amw.name).size == 2) {
          val idx: Int = amwPolicies(amw.name).indexWhere(w => amw.start == w.start && amw.end == w.end && amw.name == w.name)
          if (noDirChangeData(amw.name)._2 && idx <= noDirChangeData(amw.name)._1) {
            amw.copy(speed = -amw.speed.sign * 2.0)
          }
          else if (!noDirChangeData(amw.name)._2 && noDirChangeData(amw.name)._1 < idx) {
            amw.copy(speed = -amw.speed.sign * 2.0)
          }
          else {
            amw
          }
        }
        else {
          amw
        }
      }
      case other: ControlDevicePolicy => {
        other
      }
    }
    tmp
  }
}

object MinimumDurationSameDirection extends OperatorGenerator with RandomChange {
  val name: String = "ChangeDirectionBlock"

  type T = MinimumDurationSameDirection

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = new MinimumDurationSameDirection(x.x)
}


class DownstreamDensityUpdate(amwAreas: Map[String, (Vector[String], Vector[String])], densityDataBySimulation: Vector[Map[(String, Int), Double]], val timeIntervals: Vector[Time], fraction: Option[Double]) extends Operator {

  val densityData/*: Map[(String, Int), Double]*/ = densityDataBySimulation
    .flatten
    .groupBy(_._1)
    .view
    .mapValues(v => v.map(_._2).sum/v.size.toDouble).to(Map).toVector
    .map(kv => (kv._1._1, timeIntervals.sliding(2).indexWhere(i => i.head <= timeIntervals(kv._1._2) && timeIntervals(kv._1._2) <= i.last)) -> kv._2*2.5)
    .groupBy(_._1)
    .mapValues(v => v.map(_._2).sum).to(Map)


  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {
    val tmp = x.map {
      case amw: AMWPolicy if ThreadLocalRandom.current().nextDouble() < fraction.getOrElse(0.5) => {
        val startDensity = densityData.find(t => t._1 == (amwAreas(amw.name)._1.head, timeIntervals.indexOf(amw.start)))
        val endDensity = densityData.find(t => t._1 == (amwAreas(amw.name)._2.head, timeIntervals.indexOf(amw.start)))
        if (startDensity.isDefined && endDensity.isDefined && startDensity.get._2 >= endDensity.get._2) {
          amw.copy(speed = 3.0)
        }
        else if (startDensity.isDefined && endDensity.isDefined && startDensity.get._2 < endDensity.get._2) {
          amw.copy(speed = -3.0)
        }
        else if (startDensity.isDefined && endDensity.isEmpty) {
          amw.copy(speed = 3.0)
        }
        else if (startDensity.isEmpty && endDensity.isDefined) {
          amw.copy(speed = -3.0)
        }
        else {
          amw.copy(speed = 3.0)
        }
      }
      case other => other
    }
    tmp
  }
}

object DownstreamDensityUpdate extends OperatorGenerator with RandomChange {
  val name: String = "DownstreamDensityUpdate"

  type T = DownstreamDensityUpdate

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new DownstreamDensityUpdate(iterable.head.densitiesInsideAreas.amwsZones, iterable.map(_.densitiesInsideAreas.quantile75DensityByArea), iterable.head.intervals, params.fraction)
  }
}

  /** Randomly set constant speed of moving walkways.
    * For each moving walkway in the infrastructure, there is a 50% chance of ŝelecting it for changing the speed.
    * For each moving walkway which has been selected for a speed change, a speed is sampled uniformly from the
    * set of possible speeds. The other speeds are lesft unchanged.
    */
class RandomSetSpeed(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val policyByAMW: Map[String, Vector[ControlDevicePolicy]] = x.groupBy(_.name)

    val amwToChange: Vector[String] = policyByAMW.keys.filter(n => ThreadLocalRandom.current().nextDouble() < fraction.getOrElse(0.5)).toVector

    val possibleSpeeds: Vector[Double] = (BigDecimal(MINIMUM_SPEED) to BigDecimal(MAXIMUM_SPEED) by SPEED_INCREMENT).map(_.toDouble).toVector


    (policyByAMW.collect{
      case policy if amwToChange.contains(policy._1) => {
        val newSpeed = Random.shuffle(possibleSpeeds).head
        policy._2.collect{
          case amw: AMWPolicy => amw.copy(speed = newSpeed)
          case other => other
        }
      }
      case other => other._2
    }).flatten.toVector
  }
}

object RandomSetSpeed extends OperatorGenerator with RandomChange {
  val name: String = "RandomSetSpeed"

  type T = RandomSetSpeed

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new RandomSetSpeed(params.fraction)
  }
}


class RandomSetSpeedSpeedUpdates(fraction: Option[Double]) extends Operator {

  def xprime(x: Vector[ControlDevicePolicy]): Vector[ControlDevicePolicy] = {

    val setSpeed: RandomSetSpeed = new RandomSetSpeed(None)

    val increaseSpeed: RandomIncreaseSpeed = new RandomIncreaseSpeed(fraction)
    val decreaseSpeed: RandomDecreaseSpeed = new RandomDecreaseSpeed(fraction)

    increaseSpeed.xprime(decreaseSpeed.xprime(setSpeed.xprime(x)))

  }
}

object RandomSetSpeedSpeedUpdates extends OperatorGenerator with RandomChange {
  val name: String = "RandomSetSpeedCombinedSpeedUpdates"

  type T = RandomSetSpeedSpeedUpdates

  def returnOperator(x: Policy, iterable: Vector[StateGroundTruthPredicted], params: OperatorParameters): T = {
    new RandomSetSpeedSpeedUpdates(params.fraction)
  }
}