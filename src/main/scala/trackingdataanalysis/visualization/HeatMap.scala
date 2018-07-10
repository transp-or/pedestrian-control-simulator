package trackingdataanalysis.visualization

import java.awt.Graphics2D
import java.awt.image.BufferedImage

import visualization.PlotOptions
import hubmodel.output.createWhiteBackground

class HeatMap(outputFile: String,
              data: Iterable[(Double, Double, Double)],
              opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) with VisualizationTools {

  // finds the intervals in data
  val xInterval: Double = {
    val distinctGaps: Vector[Double] = data.map(_._1).dropRight(1).zip(data.map(_._1).tail).map(t => t._2 - t._1).toVector.distinct
    if (distinctGaps.size != 1) {
      throw new RuntimeException("Gaps in x direction of data for heatmap are not constant !")
    }
    distinctGaps(0)
  }
  val yInterval: Double = {
    val distinctGaps: Vector[Double] = data.map(_._2).dropRight(1).zip(data.map(_._2).tail).map(t => t._2 - t._1).toVector.distinct
    if (distinctGaps.size != 1) {
      throw new RuntimeException("Gaps in y direction of data for heatmap are not constant !")
    }
    distinctGaps(0)
  }

  // minimum and maximum of each axis
  val xMin: Double = data.map(_._1).min - 0.5*xInterval
  val yMin: Double = data.map(_._2).min - 0.5*yInterval
  val xMax: Double = data.map(_._1).max + 0.5*xInterval
  val yMax: Double = data.map(_._2).max + 0.5*yInterval

  // create white background
  val canvas: BufferedImage = createWhiteBackground((xMax-xMin, yMax-yMin))

  // completes abstract classes by implementing the mapping functions
  override def mapHCoord: (Time) => Int = mapHcoordLinear(xMin, xMax, opts.width-opts.border2HorizontalAxis-opts.border2VerticalAxis)
  override def mapVCoord: (Time) => Int = mapVcoordLinear(0, 1.2*yMax, opts.height-2*opts.border2HorizontalAxis)
  override def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)

  val gCanvas: Graphics2D = canvas.createGraphics()

  drawHeatMap(gCanvas, data, xInterval, yInterval)

}
