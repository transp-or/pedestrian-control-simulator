package trackingdataanalysis.visualization

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage

import visualization.PlotOptions
import javax.imageio.ImageIO

class HeatMap(outputFile: String,
              rawData: Iterable[(Double, Double, Double)],
              label: String,
              xLabel: String,
              yLabel: String,
              opts: PlotOptions = PlotOptions()) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) {

  val data: Iterable[(Double, Double, Double)] = rawData.filterNot(_._3.isNaN)

  // finds the intervals in data
  val xInterval: Double = {
    val xCoordsDistinct: Vector[Double] = data.map(r => r._1).toVector.distinct.sorted
    xCoordsDistinct.size match {
      case 0 => throw new RuntimeException("no data in x direction ! maybe only NaNs are present ?")
      case 1 => xCoordsDistinct.head
      case _ => xCoordsDistinct.dropRight(1).zip(xCoordsDistinct.tail).map(t => t._2 - t._1).distinct.min
    }
  }

   val yInterval: Double = {
     val yCoordsDistinct: Vector[Double] = data.map(_._2).toVector.distinct.sorted
     yCoordsDistinct.size match {
       case 0 => throw new RuntimeException("no data in y direction ! maybe only NaNs are present ?")
       case 1 => yCoordsDistinct.head
       case _ => yCoordsDistinct.dropRight(1).zip(yCoordsDistinct.tail).map(t => t._2 - t._1).distinct.min
     }
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
  override def mapVCoord: (Time) => Int = mapVcoordLinear(yMin, yMax, opts.height-2*opts.border2HorizontalAxis)
  override def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)


  def minSkipNaN(data: Iterable[Double]): Double = {data.filterNot(x => x.isNaN || x.isInfinity).min}
  def maxSkipNaN(data: Iterable[Double]): Double = {data.filterNot(x => x.isNaN || x.isInfinity).max}

  drawAxisShifted(gCanvas, (xMin, yMin), Some(xMin+0.5*xInterval, xMax-0.5*xInterval, xInterval,xLabel), Some(yMin+0.5*yInterval, yMax-0.5*yInterval, yInterval,yLabel))
  drawHeatMap(gCanvas, data, (xInterval, data.map(_._1).toVector.distinct.size), (yInterval, data.map(_._2).toVector.distinct.size), opts.zmin, opts.zmax)
  drawColorMapLegend(gCanvas, if (opts.zmin.isDefined) opts.zmin.get else minSkipNaN(data.map(_._3)), if (opts.zmax.isDefined) opts.zmax.get else maxSkipNaN(data.map(_._3)), label)

  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}
