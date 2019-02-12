package hubmodel.tools.cells

import hubmodel.{Position, Time}


class DensityMeasuredArea(name: String, A: Position, B: Position, C: Position, D: Position, val targetDensity: Double) extends Rectangle(name, A, B, C, D, false, Some(0)) {

  // pedestrian density.
  val densityHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()

  // density per pedestrian computed using voronoi tessellations.
  val paxIndividualDensityHistory: collection.mutable.ArrayBuffer[(Time, Iterable[Double])] = collection.mutable.ArrayBuffer()

  // inflow into the controlled area.
  val inflowHistory: collection.mutable.ArrayBuffer[(Time, Double)] = collection.mutable.ArrayBuffer()

  /**
    * Initializes the containers at the start of the simulation with values of 0. This assume that the simulation is
    * started allways from an empty state.
    *
    * @param startTime time of the first measurement
    */
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
