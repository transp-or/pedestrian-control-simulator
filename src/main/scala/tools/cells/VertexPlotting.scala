package tools.cells

import java.awt.Color

trait VertexPlotting extends Vertex {

  def scalarToShow: Double

  def scalarToColor: Color = {
    if (this.scalarToShow >= 2.17) new Color(153, 0, 0, 25)
    else if (this.scalarToShow > 1.08) new Color(255, 0, 0, 25)
    else if (this.scalarToShow > 0.72) new Color(255, 128, 0, 25)
    else if (this.scalarToShow > 0.43) new Color(255, 255, 0, 25)
    else if (this.scalarToShow > 0.31) new Color(0, 255, 0, 25)
    else if (this.scalarToShow <= 0.31) new Color(0, 0, 255, 25)
    else Color.WHITE
  }

  def stringToShow: String

  def horizontalMaxTextWidth: Double

  def xCoords: Array[Double] = this.corners.map(_.X).toArray

  def yCoords: Array[Double] = this.corners.map(_.Y).toArray
}