package hubmodel.io.output.video

import java.awt.geom.Path2D
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File

import hubmodel.io.output.{mapCoordAffine, verticalMirrorTransformation}
import hubmodel.tools.Time
import org.jcodec.api.awt.AWTSequenceEncoder

import scala.collection.immutable.NumericRange

class KPIEvolutionOverTime(outputFile: String,
                           size: (Int, Int),
                           fps: Int,
                           var data: collection.mutable.ArrayBuffer[(Time, Double)],
                           times2Show: Range) extends Tools4Videos {

  val borderSpacing: Int = 40
  val canvas: BufferedImage = new BufferedImage(size._1, size._2, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapCoordAffine(times2Show.end, times2Show.head, canvas.getWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapCoordLinear(1.5, canvas.getHeight - 2 * borderSpacing)

  /** Swaps the vertical coordinate, making the origin at the bottom left
    *
    * @return function taking a pixel and returning the swapped pixel
    */
  def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)


  def drawRotate(g2d: Graphics2D, x: Double, y: Double, angle: Int, text: String): Unit = {
    g2d.translate(x, y)
    g2d.rotate(Math.toRadians(angle))
    g2d.drawString(text, 0, 0)
    g2d.rotate(-Math.toRadians(angle))
    g2d.translate(-x, -y)
  }

  gCanvas.setColor(Color.BLACK)

  // axis
  gCanvas.drawLine(borderSpacing, verticalTransformation(borderSpacing), canvas.getWidth - borderSpacing + 20, verticalTransformation(borderSpacing))
  gCanvas.drawLine(borderSpacing, verticalTransformation(borderSpacing), borderSpacing, verticalTransformation(canvas.getHeight - borderSpacing + 20))

  // arrow heads
  gCanvas.drawLine(canvas.getWidth - borderSpacing + 20, verticalTransformation(borderSpacing), canvas.getWidth - 60 + 30, verticalTransformation(45))
  gCanvas.drawLine(canvas.getWidth - borderSpacing + 20, verticalTransformation(borderSpacing), canvas.getWidth - 60 + 30, verticalTransformation(35))
  gCanvas.drawLine(borderSpacing, verticalTransformation(canvas.getHeight - borderSpacing + 20), 45, verticalTransformation(canvas.getHeight - 60 + 30))
  gCanvas.drawLine(borderSpacing, verticalTransformation(canvas.getHeight - borderSpacing + 20), 35, verticalTransformation(canvas.getHeight - 60 + 30))

  // axis labels
  gCanvas.drawString("time", scala.math.floor(canvas.getWidth * 0.5).toInt, verticalTransformation(10))
  drawRotate(gCanvas, 12, verticalTransformation(scala.math.floor(canvas.getHeight * 0.5).toInt), 270, "density") //, 20, verticalTransformation(200))

  // xticks
  val nXTicks: Int = 5
  val nYTicks: Int = 5
  val xTicks: Range = times2Show.head to times2Show.end by (times2Show.end - times2Show.head) / nXTicks
  val yTicks: NumericRange[BigDecimal] = BigDecimal(0.0) to 1.5 by (1.5 - 0.0) / nYTicks
  val posXTicks: IndexedSeq[(Int, Int)] = xTicks map (mapHcoord(_)) zip xTicks
  val posYTicks: IndexedSeq[(Int, Double)] = yTicks.map(v => mapVcoord(v.toDouble)) zip yTicks.map(_.toDouble)

  posYTicks.foreach(tick => {
    gCanvas.drawLine(40, verticalTransformation(borderSpacing + tick._1), 45, verticalTransformation(borderSpacing + tick._1))
    gCanvas.drawString(f"${tick._2}%1.1f", 15, verticalTransformation(borderSpacing + tick._1 - scala.math.floor(0.33 * gCanvas.getFontMetrics.getHeight).toInt))
  })

  posXTicks.foreach(tick => {
    gCanvas.drawLine(borderSpacing + tick._1, verticalTransformation(35), borderSpacing + tick._1, verticalTransformation(45))
    gCanvas.drawString((tick._2.toString), borderSpacing + tick._1 - scala.math.floor(0.5 * gCanvas.getFontMetrics.stringWidth((tick._2.toString))).toInt, verticalTransformation(20))
  })

  gCanvas.setColor(Color.RED)

  val plot: Path2D = new Path2D.Double()
  plot.moveTo(borderSpacing + mapHcoord(times2Show.head), verticalTransformation(mapVcoord(0)) - borderSpacing)

  val enc: AWTSequenceEncoder = AWTSequenceEncoder.createSequenceEncoder(new File(outputFile), fps)

  for (i <- times2Show.indices) {

    //if (100*times2Show(i)/(times2Show.end-times2Show.head).toInt % 5 == 0) {println(100*times2Show(i)/(times2Show.end-times2Show.head))}

    plot.lineTo(borderSpacing + mapHcoord(times2Show(i)), verticalTransformation(mapVcoord(data.head._2)) - borderSpacing)
    gCanvas.draw(plot)

    if (data.tail.head._1.value == times2Show(i)) {
      data = data.tail
    }
    enc.encodeImage(canvas)
  }
  enc.finish()
}