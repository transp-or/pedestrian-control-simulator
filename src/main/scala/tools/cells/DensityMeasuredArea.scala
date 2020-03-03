package tools.cells

import hubmodel.Position
import hubmodel.control.ControlDeviceComponent
import tools.Time

class DensityMeasuredArea(name: String, A: Position, B: Position, C: Position, D: Position, val targetDensity: Double) extends Rectangle(name, A, B, C, D, false, Some(0)) with ControlDeviceComponent {

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

  private type U = DensityMeasuredArea

  /** Deep copy of this monitored area.
    *
    * @return deep copy of the current component
    */
  override def deepCopy: U = new DensityMeasuredArea(name, A, B, C, D, targetDensity)

  /** Makes a deep copy of this monitored area but changes the target density to the value passed as a parameter.
    *
    * @param rho target density value to use
    * @return deep copy of the current monitored area with changed target density
    */
  def deepCopyChangeTargetDensity(rho: Double) = new DensityMeasuredArea(name, A, B, C, D, rho)


  def toJSON: String = {
    "{" +
      "\"name\":\"" + this.name + "\"," +
      "\"target-density\":" + this.targetDensity + "," +
      "\"density-measurements\":[" +
    this.densityHistory.map(dp => "{\"time\":" + dp._1.toString + ",\"density\":" + dp._2 + "}").mkString(",\n") +
    "],\n\"density-individual-measurements\":[" + this.paxIndividualDensityHistory.map(d => "{\"time\":" + d._1 + ",\"densities\":[" + d._2.mkString(",")) + "]}"+
    "]"+
      "}"
  }
}
