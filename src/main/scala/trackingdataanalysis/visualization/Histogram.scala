package trackingdataanalysis.visualization

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.imageio.ImageIO
import visualization.PlotOptions

import scala.collection.immutable.NumericRange

import myscala.math

class Histogram(outputFile: String,
                x: Iterable[Double],
                binSize: Double,
                xLabel: String,
                title: String,
                opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) with VisualizationTools {

  if (x.isEmpty) { throw new IllegalArgumentException("Data for histogram is empty !")}

  val xmin: Double = if (opts.xmin.isDefined) opts.xmin.get else x.min
  val xmax: Double = if (opts.xmax.isDefined) opts.xmax.get else x.max

  // process data
  val intervals: NumericRange[Double] = xmin.to(xmax).by(binSize)
  def binningFunc(v: Double): Int = intervals.indexWhere( _ > v)

  val binnedData: Vector[(Int, Double)] =  x.groupBy(binningFunc).map(kv => kv._1-1 -> kv._2.size.toDouble/x.size).toVector.sortBy(_._1)

  // completes abstract classes by implementing the mapping functions
   def mapHCoord(v: Double): Int = mapHcoordLinear(xmin, xmax, opts.width-opts.border2HorizontalAxis-opts.border2VerticalAxis)(v)
   def mapVCoord(v: Double): Int = mapVcoordLinear(if (opts.ymin.isDefined) opts.ymin.get else 0.0, if (opts.ymax.isDefined) opts.ymax.get else 1.2*binnedData.map(_._2).max, opts.height-2*opts.border2HorizontalAxis)(v)//1.2*binnedData.map(_._2).max(v)
  override def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)

  def mapHCoordBD(x: BigDecimal): Int = 0
  def mapHcoordDrawingZone(x: Double): Int = 0
  def mapVcoordDrawingZone(x: Double): Int = 0
  def mapHcoordDrawingZoneBD(x: BigDecimal): Int = 0
  def mapVcoordDrawingZoneBD(x: BigDecimal): Int = 0
  def mapVCoordBD(x: BigDecimal): Int = 0


  // builds the background based on the size passed as argument
  val canvas: BufferedImage = new BufferedImage(opts.width, opts.height, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  drawAxis(gCanvas, None, Some((if (opts.ymin.isDefined) opts.ymin.get else 0.0, if (opts.ymax.isDefined) opts.ymax.get else 1.2*binnedData.map(_._2).max, 0.01, "frequency"))) // 1.2*binnedData.map(_._2).max,1.2*binnedData.map(_._2).max/10.0
  drawHistogram(gCanvas, binnedData, intervals.toVector, xLabel)
  drawTitle(gCanvas, title)

  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}
