package optimization

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.control.amw.{AMWPolicy, MovingWalkwayControlEvents}
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import hubmodel.AMW_ACCELERATION_AMPLITUDE

import scala.annotation.tailrec


package object ALNS {

  type Iteration = Int
  type Temperature = Double
  type OperatorName = String

  type FunctionEvaluation = Map[String, Vector[Double]]

  type FunctionEvaluationReduced = Map[String, Double]

  type Solution = (Vector[ControlDevicePolicy], Vector[ControlDeviceData])

  type AMWSpeedIdx = (String, Int)

  type AMWSpeed = (AMWSpeedIdx, Double)

  type ALNSPoint = Vector[AMWSpeed]

  type AMWSpeedRange = Set[Double]

  val SPEED_INCREMENT: Double = 0.25

  val MINIMUM_SPEED: Double = -3.0
  val MAXIMUM_SPEED: Double = 3.0


  def groupAMWPolicies(x: Vector[ControlDevicePolicy]): (Map[String, Vector[AMWPolicy]], Vector[ControlDevicePolicy]) = {
    val (amw, others) = x.partitionMap {
      case amw: AMWPolicy => Left(amw)
      case other: ControlDevicePolicy => Right(other)
    }

    val amwPolicies: Map[String, Vector[AMWPolicy]] = amw
      .groupBy(_.name)
      .view.mapValues(_.sortBy(_.start)).to(Map)

    (amwPolicies, others)
  }

  def roundToSpeedValues(x: Double): Double = {
    (x * 4.0).round / 4.0
  }

  def enforceSpeedChangeIntoPolicy(x: Vector[ControlDevicePolicy], initialAMWSpeed: Map[String, Double]): (Vector[ControlDevicePolicy], Vector[MovingWalkwayControlEvents]) = {

    /** Returns the indices where there is a change in direction in the AMW.
      *
      * @param x control policy
      * @return indices where the speed direction changes
      */
    def findDirectionChanges(x: Vector[AMWPolicy]): Vector[Int] = {
      if (x.size > 1) {
        x.sliding(2)
          .zipWithIndex
          .collect { case change if (
            change._1.head.speed > 0 && change._1.last.speed < 0) ||
            (change._1.head.speed < 0 && change._1.last.speed > 0) /*&&
        math.abs(change._1.head.speed) + math.abs(change._1.last.speed) > ((change._1.last.end - change._1.last.start).value.toDouble*AMW_ACCELERATION_AMPLITUDE)*/ => change._2 + 1
          }
          .toVector
      } else { Vector()}
    }

    /** Returns the indices where the acceleration is too large, i.e. when the change in speed over the duration of the
      * policy interval is greater than [[AMW_ACCELERATION_AMPLITUDE]]
      *
      * @param x policy to check
      * @return indices where acceleration is too large
      */
    def findExcessiveAcceleration(x: Vector[AMWPolicy]): Vector[Int] = {
      {if (math.abs((x.head.speed - initialAMWSpeed(x.head.name)))/(x.head.end - x.head.start).value > AMW_ACCELERATION_AMPLITUDE) {Vector(0)} else {Vector()}} ++ {if (x.size > 1) {
        x.sliding(2)
          .zipWithIndex
          .collect { case acc if math.abs(acc._1(1).speed - acc._1(0).speed) / (acc._1(1).start - acc._1(0).start).value > AMW_ACCELERATION_AMPLITUDE => acc._2 + 1 }
          .toVector
      } else {Vector()}}
    }

    /** Finds all indices corresponding to policies which must be adapated.
      *
      * @param x initial control policy
      * @return indicies where the policy must be adapated
      */
    def findIndicesToChange(x: Vector[AMWPolicy]): Vector[Int] = {
      (findDirectionChanges(x) ++ findExcessiveAcceleration(x)).distinct.sorted
    }


    @tailrec def rec(directionChanges: Vector[Int], policy: Vector[AMWPolicy], amwEvents: MovingWalkwayControlEvents): (Vector[AMWPolicy], MovingWalkwayControlEvents) = {

      def changeAMWPolicy(dirChange: AMWPolicy, amwTripTime: Time, speedChangeStart: Time, speedChangeEnd: Time, oldSpeed: Double, newSpeed: Double)(x: AMWPolicy, previousPolicySpeed: Option[Double]): AMWPolicy = {

        /*val close: Option[Time] = if (dirChange.start == x.start && dirChange.end == x.end) {
          Some(dirChange.start)
        } else {
          None
        }

        val open: Option[Time] = if (dirChange.start == x.start && dirChange.end == x.end) {
          Some(dirChange.start + amwTripTime + Time(math.abs(oldSpeed / AMW_ACCELERATION_AMPLITUDE)))
        } else {
          None
        }*/

        x match {
          // delay to allow pedestrian to leave the amw completely spans over the policy interval.
          case amw: AMWPolicy if amw.name == dirChange.name && amw.end <= speedChangeStart && amw.speed.sign != oldSpeed.sign && amw.start >= dirChange.start => {
            amw.copy(speed = previousPolicySpeed.get)
          }
          // before speed change has started
          case amw: AMWPolicy if amw.name == dirChange.name && amw.end <= speedChangeStart => {
            amw
          }
          // after change speed has ended
          case amw: AMWPolicy if amw.name == dirChange.name && amw.start >= speedChangeEnd => {
            amw
          }
          // interval when the speed changes ends
          case amw: AMWPolicy if amw.name == dirChange.name && amw.start < speedChangeEnd && amw.end > speedChangeEnd => {
            amw.copy(speed = newSpeed)
          }
          // interval where the speed changes begins
          case amw: AMWPolicy if amw.name == dirChange.name && (amw.start < speedChangeStart && speedChangeStart < amw.end) && amw.end >= speedChangeStart => {
            amw.copy(speed = previousPolicySpeed.get + roundToSpeedValues(hubmodel.AMW_ACCELERATION_AMPLITUDE * (amw.end - speedChangeStart).value.toDouble) * (dirChange.speed.sign))
          }
          // intervals for which the speed change is happening accross all interval
          case amw: AMWPolicy if amw.name == dirChange.name && amw.start >= speedChangeStart && amw.start < speedChangeEnd && amw.end < speedChangeEnd => {
            amw.copy(speed = previousPolicySpeed.get + roundToSpeedValues(hubmodel.AMW_ACCELERATION_AMPLITUDE * (amw.end - amw.start).value.toDouble) * (dirChange.speed.sign))
          }
          // non affected control policies
          case other: AMWPolicy => {
            other
          }
        }
      }


      @tailrec def processPolicy(changeAMWPolicy: (AMWPolicy, Option[Double]) => AMWPolicy)(tmpPolicy: Vector[AMWPolicy], idxToProcess: Int): Vector[AMWPolicy] = {
        if (idxToProcess == tmpPolicy.size) {
          tmpPolicy
        } else {
          val changedPolicy: Vector[AMWPolicy] = tmpPolicy.take(idxToProcess) ++
            Vector(
              changeAMWPolicy(
                tmpPolicy(idxToProcess),
                if (idxToProcess - 1 >= 0) {Some(tmpPolicy(idxToProcess - 1).speed)}
                else {Some(initialAMWSpeed(tmpPolicy(idxToProcess).name))}
              )
            ) ++ tmpPolicy.takeRight(tmpPolicy.size - 1 - idxToProcess)

          processPolicy(changeAMWPolicy)(changedPolicy, idxToProcess + 1)
        }
      }


      if (directionChanges.isEmpty) {
        (policy, amwEvents)
      } else {
        // policy which must be changed
        val dirChange = policy(directionChanges.head)

        // previous speed which won't be changed
        val oldSpeed = {
          if (directionChanges.head > 0) {
            policy(directionChanges.head - 1).speed // previous speed is in policy
          } else {
            initialAMWSpeed(dirChange.name) // previous speed is before policy
          }
        }
        // original target speed
        val newSpeed: Double = dirChange.speed

        val requireCloseOpenEvents: Boolean = (oldSpeed < 0.0 && newSpeed > 0.0 ) || (oldSpeed > 0.0 && newSpeed < 0.0)

        // expected trip duration on the amw
        val amwTripTime: Time = Time(dirChange.amwLength / (1.34))

        // time to close the entrance of the amw, i.e. the time when the route graph must stop pedestrians from
        // walking along this link
        val routeGraphCloseTime: Option[Time] = if (requireCloseOpenEvents) {Some(dirChange.start)} else {None}

        // time when the moving walkway can start to slow down. This should allow all pedestrians to clear the amw.
        val speedChangeStart: Time = if (requireCloseOpenEvents) {routeGraphCloseTime.get + amwTripTime } else {dirChange.start}

        // time when the moving walkway can allow pedestrians along it again since it's now moving in the right direction
        val routeGraphOpenTime: Option[Time] = if (requireCloseOpenEvents) {Some(routeGraphCloseTime.get + amwTripTime + Time(math.abs(oldSpeed / AMW_ACCELERATION_AMPLITUDE)))} else {None}

        // time when the moving walkway will have reached it's target speed.
        val speedChangeEnd: Time = speedChangeStart + Time(math.abs((newSpeed - oldSpeed) / hubmodel.AMW_ACCELERATION_AMPLITUDE))


        val newPolicy: Vector[AMWPolicy] = {
          if (routeGraphOpenTime.isDefined && amwEvents.openTime.contains(routeGraphOpenTime.get) && routeGraphCloseTime.isDefined && amwEvents.closeTime.contains(routeGraphCloseTime.get)) {
            println("nothing to do ! ")
            policy
          } else {
            //println("need to change policy")
            // function which will update the one single policy to match the close and open times with feasible acceleration
            val updatePolicy = changeAMWPolicy(dirChange, amwTripTime, speedChangeStart, speedChangeEnd, oldSpeed, newSpeed) _

            // function which wlaks thtough the whole control policy and applies the "updatePolicy" function to each interval
            processPolicy(updatePolicy)(policy, 0)
          }
        }

        val newPolicy2 = {
          if (newPolicy.size == 1) {
            Vector(newPolicy.head.copy(_s = newPolicy.head.start + amwTripTime))
          } else {
            newPolicy
          }
        }

        // update the direction changes and then proceed to the next one
        if (routeGraphCloseTime.isDefined && routeGraphOpenTime.isDefined) {
          rec(findIndicesToChange(newPolicy2).filter(_ > directionChanges.head + 1), newPolicy2, amwEvents.copy(closeTime = amwEvents.closeTime :+ routeGraphCloseTime.get, openTime = amwEvents.openTime :+ routeGraphOpenTime.get))}
        else {
          rec(findIndicesToChange(newPolicy2).filter(_ > directionChanges.head + 1), newPolicy2, amwEvents)}
      }
    }


    val (amwPolicies, other): (Map[String, Vector[AMWPolicy]], Vector[ControlDevicePolicy]) = groupAMWPolicies(x)

    val directionChanges: Map[String, Vector[Int]] = amwPolicies
      .map(g => g._1 -> {
        if ((initialAMWSpeed(g._1).sign < 0 && g._2.head.speed.sign > 0) || (initialAMWSpeed(g._1).sign > 0 && g._2.head.speed.sign < 0)) {
          0 +: findIndicesToChange(g._2)
        } else {
          findIndicesToChange(g._2)
        }
      })

    val tmp: Map[String, (Vector[AMWPolicy], MovingWalkwayControlEvents)] = amwPolicies.map(amwP => {
      amwP._1 -> rec(directionChanges(amwP._1), amwP._2, MovingWalkwayControlEvents(amwP._1))
    })

    (
      other ++ tmp.flatMap(_._2._1), tmp.map(_._2._2).toVector
    )
  }
}
