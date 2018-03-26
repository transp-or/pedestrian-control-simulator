package hubmodel.tools.cells

import java.awt.Color

trait NewVertexPlotting extends NewVertex {
  def scalarToShow: Double
  def scalarToColor: Color
  def stringToShow: String
  def horizontalMaxTextWidth: Double
  def xCoords: Array[Double] = this.corners.map(_.X).toArray
  def yCoords: Array[Double] = this.corners.map(_.Y).toArray

}