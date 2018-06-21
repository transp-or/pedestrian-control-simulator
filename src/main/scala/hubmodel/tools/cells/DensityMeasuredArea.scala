package hubmodel.tools.cells

import hubmodel.{Position, Time}


class DensityMeasuredArea(name: String, A: Position, B: Position, C: Position, D: Position, startTime: Time) extends Rectangle(name, A, B, C, D) {
  val densityHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))
  val inflowHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer((startTime, 0.0))
  var regulatorIntegralAction: Double = 0.0
}
