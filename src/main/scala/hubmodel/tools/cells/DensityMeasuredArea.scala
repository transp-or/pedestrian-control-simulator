package hubmodel.tools.cells

import hubmodel.{Position, Time}


class DensityMeasuredArea(name: String, A: Position, B: Position, C: Position, D: Position, val targetDensity: Double) extends Rectangle(name, A, B, C, D) {

  // pedestrian density.
  val densityHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()

  // density per pedestrian computed using voronoi tessellations.
  val paxIndividualDensityHistory: collection.mutable.ArrayBuffer[(Time, Iterable[Double])] = collection.mutable.ArrayBuffer()

  // inflow inot the controlled area.
  val inflowHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()

  def initializeContainers(startTime: Time): Unit = {
    densityHistory.append((startTime, 0.0))
    inflowHistory.append((startTime, 0.0))
    paxIndividualDensityHistory.append((startTime, Vector()))
  }

  //var regulatorIntegralAction: Double = 0.0

  override def clone(): DensityMeasuredArea = new DensityMeasuredArea(
    name, A, B, C, D, targetDensity
  )
}
