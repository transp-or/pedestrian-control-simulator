package hubmodel.route.Flurin2014

import hubmodel.Position
import hubmodel.supply.potential.PotentialCell
import hubmodel.tools.cells.{Rectangle, Square, VertexPlotting}

class PotentialSquareCell(A: Position, B: Position, C: Position, D: Position, surface: Double) extends Square(A, B, C, D, false, None) with PotentialCell with VertexPlotting {

  /** Constructor using only the center and side lenght to build the square
    *
    * @param center
    * @param side
    */
  def this(center: Position, side: Double, surface: Double) {
    this(
      center + new Position(-0.5 * side, -0.5 * side),
      center + new Position(0.5 * side, -0.5 * side),
      center + new Position(0.5 * side, 0.5 * side),
      center + new Position(-0.5 * side, 0.5 * side),
      surface
    )
  }

  def this(center: Position, side: Double) {
    this(
      center + new Position(-0.5 * side, -0.5 * side),
      center + new Position(0.5 * side, -0.5 * side),
      center + new Position(0.5 * side, 0.5 * side),
      center + new Position(-0.5 * side, 0.5 * side),
      side*side
    )
  }

  override val area: Double = surface

  val criticalDensity: Double = 5.4

  val shape: Double = 1.913

  val maxOccupancy: Double = criticalDensity * this.area

  var currentOccupancy: Int = 0

  var staticPotential: Option[Double] = Some(0)
  var dynamicPotential: Option[Double] = Some(0)

  override def potential: Double = staticPotential.get + dynamicPotential.get

  def computeSpeed: Double = {
    if (this.currentOccupancy >= 1) { // if at least one pedestrian is present inside the cell
      1.0 - math.exp(-this.shape * this.area * ((1.0/this.currentOccupancy) - 1.0/this.maxOccupancy))
    } else { // speed isn't defined when no pedestrians are found in the cell
      1.0
    }
  }

  def horizontalMaxTextWidth: Double = (B-A).norm


  def stringToShow: String = {
    "%1.2f".format(this.area)//+ " / " + "%1.2f".format(this.potential)
  }

  def scalarToShow: Double =  this.area


}
