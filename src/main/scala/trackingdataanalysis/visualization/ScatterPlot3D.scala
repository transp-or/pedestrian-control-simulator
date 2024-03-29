package trackingdataanalysis.visualization

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.imageio.ImageIO


class ScatterPlot3D(outputFile: String,
                    x: Vector[Double],
                    y: Vector[Double],
                    z: Vector[Double],
                    xLabel: String,
                    yLabel: String,
                    opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) with VisualizationTools {

  // completes abstract classes by implementing the mapping functions
  def mapHCoord(v: Double): Int = mapHcoordLinear(x.min, x.max, opts.width - opts.border2HorizontalAxis - opts.border2VerticalAxis)(v)

  def mapVCoord(v: Double): Int = mapVcoordLinear(y.min, y.max, opts.height - 2 * opts.border2HorizontalAxis)(v)

  override def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)

  def mapHCoordBD(x: BigDecimal): Int = 0

  def mapHcoordDrawingZone(x: Double): Int = 0

  def mapVcoordDrawingZone(x: Double): Int = 0

  def mapHcoordDrawingZoneBD(x: BigDecimal): Int = 0

  def mapVcoordDrawingZoneBD(x: BigDecimal): Int = 0

  def mapVCoordBD(x: BigDecimal): Int = 0


  // checks the data
  assert(x.length == y.length, "x and y do not have the same length ! x.length=" + x.length + ", y.length=" + y.length)


  // builds the background based on the size passed as argument
  val canvas: BufferedImage = new BufferedImage(opts.width, opts.height, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  // draws the specific components
  drawScatterPoints3D(gCanvas, x, y, z)
  drawAxis(gCanvas, Some((x.min, x.max, opts.xTickInterval, xLabel)), Some((y.min, y.max, opts.yTickInterval, yLabel)))

  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}