package trackingdataanalysis.visualization

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.math.MathContext

import javax.imageio.ImageIO

class HeatMap(outputFile: String,
              rawData: Iterable[(Double, Double, Double)],
              label: String,
              xLabel: String,
              yLabel: String,
              title: String,
              opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) {

  val data: Iterable[(Double, Double, Double)] = rawData.filterNot(_._3.isNaN)

  // finds the intervals in data
  val xInterval: BigDecimal = BigDecimal.decimal({
    val xCoordsDistinct: Vector[Double] = data.map(r => r._1).toVector.distinct.sorted
    xCoordsDistinct.size match {
      case 0 => throw new RuntimeException("no data in x direction ! maybe only NaNs are present ?")
      case 1 => xCoordsDistinct.head
      case _ => xCoordsDistinct.dropRight(1).zip(xCoordsDistinct.tail).map(t => t._2 - t._1).distinct.min
    }
  }, new MathContext(5))

   val yInterval: BigDecimal = BigDecimal.decimal({
       val yCoordsDistinct: Vector[Double] = data.map(_._2).toVector.distinct.sorted
       yCoordsDistinct.size match {
         case 0 => throw new RuntimeException("no data in y direction ! maybe only NaNs are present ?")
         case 1 => yCoordsDistinct.head
         case _ => yCoordsDistinct.dropRight(1).zip(yCoordsDistinct.tail).map(t => t._2 - t._1).distinct.min
       }
     }, new MathContext(5))

  // minimum and maximum of each axis
  val xMin: BigDecimal = BigDecimal.decimal(data.map(_._1).min, new MathContext(5)) - 0.5*xInterval
  val yMin: BigDecimal = BigDecimal.decimal(data.map(_._2).min, new MathContext(5)) - 0.5*yInterval
  val xMax: BigDecimal = BigDecimal.decimal(data.map(_._1).max, new MathContext(5)) + 0.5*xInterval
  val yMax: BigDecimal = BigDecimal.decimal(data.map(_._2).max, new MathContext(5)) + 0.5*yInterval

  // builds the background based on the size passed as argument
  val canvas: BufferedImage = new BufferedImage(pixelCanvasSize._1+2*(0.075*pixelCanvasSize._1).toInt, pixelCanvasSize._2, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  // completes abstract classes by implementing the mapping functions
   def mapHCoordBD(v: BigDecimal): Int = mapHcoordLinearBD(xMin, xMax, pixelCanvasSize._1 - border2HAxis - border2VAxis)(v)
  def mapVCoordBD(v: BigDecimal): Int = mapVcoordLinearBD(yMin, yMax, pixelCanvasSize._2-2*border2HAxis)(v)
  def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)
  def mapHcoordDrawingZoneBD(v: BigDecimal): Int = mapHcoordAffineBD(xMin, xMax, border2VAxis, pixelCanvasSize._1 - border2VAxis)(v)
  def mapVcoordDrawingZoneBD(v: BigDecimal): Int = mapHcoordAffineBD(yMin, yMax, border2HAxis, pixelCanvasSize._2 - border2HAxis)(v)


  def mapHCoord(x: Double): Int = 0
  def mapHcoordDrawingZone(v: Double): Int = mapHcoordAffine(xMin.doubleValue(), xMax.doubleValue(), border2VAxis, pixelCanvasSize._1 - border2HAxis)(v)
  def mapVcoordDrawingZone(v: Double): Int = mapHcoordAffine(yMin.doubleValue(), yMax.doubleValue(), border2HAxis, pixelCanvasSize._2 - border2VAxis)(v)

  def mapVCoord(x: Double): Int = 0


  def minSkipNaN(data: Iterable[Double]): Double = {data.filterNot(x => x.isNaN || x.isInfinity).min}
  def maxSkipNaN(data: Iterable[Double]): Double = {data.filterNot(x => x.isNaN || x.isInfinity).max}

  drawAxisShiftedBD(gCanvas, (xMin, yMin), Some(xMin+0.5*xInterval, xMax-0.5*xInterval, xInterval, xLabel), Some(yMin+0.5*yInterval, yMax-0.5*yInterval, yInterval, yLabel))
  drawHeatMap(gCanvas, data, (xInterval, (xMin+ 0.5*xInterval to xMax- 0.5*xInterval by xInterval).size), (yInterval, (yMin+ 0.5*yInterval to yMax- 0.5*yInterval by yInterval).size), opts.zmin, opts.zmax)
  drawColorMapLegend(gCanvas, if (opts.zmin.isDefined) opts.zmin.get else minSkipNaN(data.map(_._3)), if (opts.zmax.isDefined) opts.zmax.get else maxSkipNaN(data.map(_._3)), label)
  drawTitle(gCanvas, title)

  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}
