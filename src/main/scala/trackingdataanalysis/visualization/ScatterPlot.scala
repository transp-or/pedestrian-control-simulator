package trackingdataanalysis.visualization

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.imageio.ImageIO


class ScatterPlot(outputFile: String,
                  x: Vector[Double],
                  y: Vector[Double],
                  xLabel: String,
                  yLabel: String,
                  opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) with VisualizationTools {

  // deals with boundaries of axis
  val xmin: Double = if (opts.xmin.isEmpty) x.min else opts.xmin.get
  val xmax: Double = if (opts.xmax.isEmpty) x.max else opts.xmax.get
  val ymin: Double = if (opts.ymin.isEmpty) y.min else opts.ymin.get
  val ymax: Double = if (opts.ymax.isEmpty) y.max else opts.ymax.get

  // completes abstract classes by implementing the mapping functions
  def mapHCoord(v: Double): Int = mapHcoordLinear(xmin, xmax, opts.width - opts.border2HorizontalAxis - opts.border2VerticalAxis)(v)

  def mapVCoord(v: Double): Int = mapVcoordLinear(ymin, ymax, opts.height - 2 * opts.border2HorizontalAxis)(v)

  override def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)

  def mapHCoordBD(x: BigDecimal): Int = 0

  def mapHcoordDrawingZone(x: Double): Int = 0

  def mapVcoordDrawingZone(x: Double): Int = 0

  def mapHcoordDrawingZoneBD(x: BigDecimal): Int = 0

  def mapVcoordDrawingZoneBD(x: BigDecimal): Int = 0

  def mapVCoordBD(x: BigDecimal): Int = 0


  // checks the data has the same size
  assert(x.length == y.length, "output file: " + outputFile + ": x and y do not have the same length ! x.length=" + x.length + ", y.length=" + y.length)

  // builds the background based on the size passed as argument
  val canvas: BufferedImage = new BufferedImage(opts.width, opts.height, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  // draws the specific components
  drawScatterPoints(gCanvas, x, y)
  drawAxis(gCanvas, Some((xmin, xmax, opts.xTickInterval, xLabel)), Some((ymin, ymax, opts.yTickInterval, yLabel)))


  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}
