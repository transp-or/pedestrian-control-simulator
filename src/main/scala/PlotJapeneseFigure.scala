package hubmodel

import hubmodel.output.image.{DrawCells, DrawCellsOverImage}
import hubmodel.tools.{MyCellTrait, Square}
import myscala.math.vector.Vector2D

object PlotJapeneseFigure extends App {

  class CellSpeedDev(c: Position, s: Double, v: Double, range: Double) extends Square(c, s) with MyCellTrait {

    val scalarToShow: Double = v
    val horizontalMaxTextWidth: Double = s
    def scalarToColor: java.awt.Color = getColorRedGreen(v/range)


    def  Color(r: Double, g: Double, b: Double, t: Double): java.awt.Color = {
      new java.awt.Color(r.toFloat, g.toFloat, b.toFloat, t.toFloat)
    }
    // Function to return a color between red and blue
    def getColorRedGreen(value: Double): java.awt.Color = {
      if (value < 0.0) {
        Color(1.0, 1.0 + value, 1.0 + value, 1.0)
      }
      else if (value >= 0.0) {
        Color(1.0-value, 1.0, 1.0-value, 1.0)
      }
      else {throw new  IllegalArgumentException("color value not in range [-1,1]")}
    }

    def stringToShow = ""//f"$v%1.2f"//.toString
  }

  val dataFile: String = "jap/gammaE_east.csv"
  val backgroundImage: String = "/home/nicholas/visiosafe-data/lausanne-metadata/zoneid-lausanne-pie.png"

  val SQUARE_SIDE_LENGTH: Double = 1.0

  val bufferedSourceRange = io.Source.fromFile(dataFile)
  val range: Double = (for (line <- bufferedSourceRange.getLines.drop(1)) yield {
    val cols = line.split(",").map(_.trim)
    // do whatever you want with the columns here
    cols(3).toDouble
  }).map(math.abs).max
  //bufferedSourceRange.close

  val bufferedSource = io.Source.fromFile(dataFile)
  val cells: Iterable[CellSpeedDev] = (for (line <- bufferedSource.getLines.drop(1)) yield {
    val cols = line.split(",").map(_.trim)
    new CellSpeedDev(Vector2D(cols(2).toDouble, cols(1).toDouble), SQUARE_SIDE_LENGTH, cols(3).toDouble, range )
  }).toIndexedSeq
  //bufferedSource.close

  val xMin: Double = cells.map(_.center.X - 0.5*SQUARE_SIDE_LENGTH).min
  val xMax: Double = cells.map(_.center.X + 0.5*SQUARE_SIDE_LENGTH).max
  val yMin: Double = cells.map(_.center.Y - 0.5*SQUARE_SIDE_LENGTH).min
  val yMax: Double = cells.map(_.center.Y + 0.5*SQUARE_SIDE_LENGTH).max

  new DrawCells(cells, "celltest.png")
    new DrawCellsOverImage(Some(backgroundImage), (0.0,28.05,62.9,47.5), cells, "cellTestWithB.png")

}
