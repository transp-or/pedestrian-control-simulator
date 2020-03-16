package optimization

import scala.collection.SortedMap

package object ALNS {

  type FunctionEvaluation = Map[String, Double]

  type AMWSpeedIdx = (String, Int)

  type AMWSpeed = (AMWSpeedIdx, Double)

  type ALNSPoint = Vector[AMWSpeed]

  type AMWSpeedRange = Set[Double]


}
