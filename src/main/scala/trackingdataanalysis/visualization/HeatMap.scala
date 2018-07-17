package trackingdataanalysis.visualization

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage

import visualization.PlotOptions
import hubmodel.output.createWhiteBackground
import javax.imageio.ImageIO

class HeatMap(outputFile: String,
              data: Iterable[(Double, Double, Double)],
              label: String,
              xLabel: String,
              yLabel: String,
              opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) {

  // finds the intervals in data
  val xInterval: Double = {
    val xCoordsDistinct: Vector[Double] = data.map(r => r._1).toVector.distinct.sorted
    val distinctGaps: Vector[Double] = xCoordsDistinct.dropRight(1).zip(xCoordsDistinct.tail).map(t => (t._2 - t._1)).distinct
    if (!distinctGaps.dropRight(1).zip(distinctGaps.tail).forall(t => math.abs(t._1 - t._2) < math.pow(10, -2))) {
      println(distinctGaps)
      throw new RuntimeException("Gaps in x direction of data for heatmap are not constant !")
    }
      distinctGaps(0)
  }

   val yInterval: Double = {
    val yCoordsDistinct: Vector[Double] = data.map(_._2).toVector.distinct.sorted
    val distinctGaps: Vector[Double] = yCoordsDistinct.dropRight(1).zip(yCoordsDistinct.tail).map(t => math.round(100.0*(t._2 - t._1))).distinct.map(_/100.0)
    if (distinctGaps.size != 1) {
      println(distinctGaps)
      throw new RuntimeException("Gaps in y direction of data for heatmap are not constant !")
    }
    distinctGaps(0)
  }

  // minimum and maximum of each axis
  val xMin: Double = data.map(_._1).min - 0.5*xInterval
  val yMin: Double = data.map(_._2).min - 0.5*yInterval
  val xMax: Double = data.map(_._1).max + 0.5*xInterval
  val yMax: Double = data.map(_._2).max + 0.5*yInterval

  // builds the background based on the size passed as argument
  val canvas: BufferedImage = new BufferedImage(opts.width+2*(0.075*opts.width).toInt, opts.height, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  // completes abstract classes by implementing the mapping functions
  override def mapHCoord: (Time) => Int = mapHcoordLinear(xMin, xMax, opts.width-opts.border2HorizontalAxis-opts.border2VerticalAxis)
  override def mapVCoord: (Time) => Int = mapVcoordLinear(yMin, 1.2*yMax, opts.height-2*opts.border2HorizontalAxis)
  override def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)

  drawAxisShifted(gCanvas, (xMin, yMin), Some(xMin+0.5*xInterval, xMax-0.5*xInterval, xInterval,xLabel), Some(yMin+0.5*yInterval, yMax-0.5*yInterval, yInterval,yLabel))
  drawHeatMap(gCanvas, data, (xInterval, data.map(_._1).toVector.distinct.size), (yInterval, data.map(_._2).toVector.distinct.size), opts.zmin, opts.zmax)
  drawColorMapLegend(gCanvas, if (opts.zmin.isDefined) opts.zmin.get else data.map(_._3).min, if (opts.zmax.isDefined) opts.zmax.get else data.map(_._3).max, label)

  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}
