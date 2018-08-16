package trackingdataanalysis.visualization

import java.awt.geom.Ellipse2D
import java.awt.{Color, Font, Graphics2D}

import breeze.linalg.min
import breeze.numerics.floor

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

abstract class DrawingComponents(xBorderSpacing: Int, yBorderSpacing: Int, pixelCanvasSize: (Int, Int)) extends VisualizationTools {

  def verticalTransformation: Int => Int

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHCoord: Double => Int// = mapHcoordLinear(bkgdImageSizeMeters._1, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVCoord: Double => Int// = mapVcoordLinear(bkgdImageSizeMeters._2, canvasHeight)


  def drawTitle(graphics2D: Graphics2D, title: String): Unit = {
    graphics2D.setColor(Color.BLACK)
    graphics2D.drawString(title, (0.5*pixelCanvasSize._1).toInt - (0.5*graphics2D.getFontMetrics.stringWidth(title)).toInt, xBorderSpacing)
  }

  def drawAxis(graphics: Graphics2D, xDataArg: Option[(Double, Double, Double, String)], yDataArg: Option[(Double, Double, Double, String)]): Unit = {

    val extraSpacing: Int = 20
    val lineExtension4Arrow: Int = 30
    graphics.setColor(Color.BLACK)

    // axis
    graphics.drawLine(yBorderSpacing, verticalTransformation(xBorderSpacing + mapVCoord(0.0)), pixelCanvasSize._1-xBorderSpacing+extraSpacing, verticalTransformation(xBorderSpacing + mapVCoord(0.0)))
    graphics.drawLine(yBorderSpacing, verticalTransformation(xBorderSpacing), yBorderSpacing, verticalTransformation(pixelCanvasSize._2-xBorderSpacing+extraSpacing))

    // arrow heads
    graphics.drawLine(pixelCanvasSize._1-xBorderSpacing+extraSpacing, verticalTransformation(xBorderSpacing + mapVCoord(0.0)), pixelCanvasSize._1-(xBorderSpacing+extraSpacing)+lineExtension4Arrow, verticalTransformation(xBorderSpacing+5 + mapVCoord(0.0)))
    graphics.drawLine(pixelCanvasSize._1-xBorderSpacing+extraSpacing, verticalTransformation(xBorderSpacing + mapVCoord(0.0)), pixelCanvasSize._1-(xBorderSpacing+extraSpacing)+lineExtension4Arrow, verticalTransformation(xBorderSpacing-5 + mapVCoord(0.0)))
    graphics.drawLine(yBorderSpacing, verticalTransformation(pixelCanvasSize._2-xBorderSpacing+extraSpacing), yBorderSpacing+5, verticalTransformation(pixelCanvasSize._2-(xBorderSpacing+extraSpacing)+lineExtension4Arrow))
    graphics.drawLine(yBorderSpacing, verticalTransformation(pixelCanvasSize._2-xBorderSpacing+extraSpacing), yBorderSpacing-5, verticalTransformation(pixelCanvasSize._2-(xBorderSpacing+extraSpacing)+lineExtension4Arrow))

    // axis labels with larger font
    val currentFont: Font = graphics.getFont
    val newFont: Font = currentFont.deriveFont(currentFont.getSize * 2.2F)
    graphics.setFont(newFont)

    if (xDataArg.isDefined) {
      graphics.drawString(xDataArg.get._4, (pixelCanvasSize._1 * 0.5 - 0.5 * graphics.getFontMetrics.stringWidth(xDataArg.get._4)).toInt, verticalTransformation(3))
    }
    if (yDataArg.isDefined) {
      drawRotate(graphics, graphics.getFontMetrics.getHeight-3, verticalTransformation(floor(pixelCanvasSize._2 * 0.5).toInt) + 0.5*graphics.getFontMetrics.stringWidth(yDataArg.get._4), 270, yDataArg.get._4)
    }
    graphics.setFont(currentFont.deriveFont(currentFont.getSize * 1.5F))

    // ticks specification
    if (xDataArg.isDefined) {
      val xTicks: NumericRange[Double] = xDataArg.get._1 to xDataArg.get._2 by xDataArg.get._3
      val posXTicks: IndexedSeq[(Int, Double)] = xTicks map mapHCoord  zip xTicks

      // draws x ticks
      posXTicks.foreach(tick => {
        graphics.drawLine(yBorderSpacing+tick._1, verticalTransformation(xBorderSpacing-5 + mapVCoord(0.0)), yBorderSpacing+tick._1, verticalTransformation(xBorderSpacing+5 + mapVCoord(0.0)))
        graphics.drawString(f"${tick._2}%1.2f", yBorderSpacing+tick._1-floor(0.5*graphics.getFontMetrics.stringWidth(f"${tick._2}%1.2f")).toInt, verticalTransformation(xBorderSpacing-5-2 + mapVCoord(0.0)-graphics.getFontMetrics.getHeight))
      })
      }
    if (yDataArg.isDefined) {
      val yTicks: NumericRange[Double] = yDataArg.get._1 to yDataArg.get._2 by yDataArg.get._3
      val posYTicks: IndexedSeq[(Int, Double)] = yTicks map mapVCoord zip yTicks

      // draws y ticks
      posYTicks.foreach(tick => {
        graphics.drawLine(yBorderSpacing-5, verticalTransformation(xBorderSpacing+tick._1), yBorderSpacing+5, verticalTransformation(xBorderSpacing+tick._1))
        graphics.drawString(f"${tick._2}%1.2f", yBorderSpacing - 5 - 2 - graphics.getFontMetrics.stringWidth(f"${tick._2}%1.2f"), verticalTransformation(xBorderSpacing+tick._1-floor(0.33*graphics.getFontMetrics.getHeight).toInt))
      })

    }
  }

  def drawAxisShifted(graphics: Graphics2D, origin: (Double, Double), xDataArg: Option[(Double, Double, Double, String)], yDataArg: Option[(Double, Double, Double, String)]): Unit = {

    val extraSpacing: Int = 20
    val lineExtension4Arrow: Int = 30
    graphics.setColor(Color.BLACK)

    // axis
    graphics.drawLine(yBorderSpacing, verticalTransformation(xBorderSpacing + mapVCoord(origin._2)), pixelCanvasSize._1-xBorderSpacing+extraSpacing, verticalTransformation(xBorderSpacing + mapVCoord(origin._2)))
    graphics.drawLine(yBorderSpacing + mapHCoord(origin._1), verticalTransformation(xBorderSpacing), yBorderSpacing + mapHCoord(origin._1), verticalTransformation(pixelCanvasSize._2-xBorderSpacing+extraSpacing))

    // arrow heads
    graphics.drawLine(pixelCanvasSize._1-xBorderSpacing+extraSpacing, verticalTransformation(xBorderSpacing + mapVCoord(origin._2)), pixelCanvasSize._1-(xBorderSpacing+extraSpacing)+lineExtension4Arrow, verticalTransformation(xBorderSpacing+5 + mapVCoord(origin._2)))
    graphics.drawLine(pixelCanvasSize._1-xBorderSpacing+extraSpacing, verticalTransformation(xBorderSpacing + mapVCoord(origin._2)), pixelCanvasSize._1-(xBorderSpacing+extraSpacing)+lineExtension4Arrow, verticalTransformation(xBorderSpacing-5 + mapVCoord(origin._2)))
    graphics.drawLine(yBorderSpacing + mapHCoord(origin._1), verticalTransformation(pixelCanvasSize._2-xBorderSpacing+extraSpacing), yBorderSpacing + mapHCoord(origin._1)+5, verticalTransformation(pixelCanvasSize._2-(xBorderSpacing+extraSpacing)+lineExtension4Arrow))
    graphics.drawLine(yBorderSpacing + mapHCoord(origin._1), verticalTransformation(pixelCanvasSize._2-xBorderSpacing+extraSpacing), yBorderSpacing + mapHCoord(origin._1)-5, verticalTransformation(pixelCanvasSize._2-(xBorderSpacing+extraSpacing)+lineExtension4Arrow))

    // axis labels with larger font
    val currentFont: Font = graphics.getFont
    val newFont: Font = currentFont.deriveFont(currentFont.getSize * 2.2F)
    graphics.setFont(newFont)

    if (xDataArg.isDefined) {
      graphics.drawString(xDataArg.get._4, (pixelCanvasSize._1 * 0.5 - 0.5 * graphics.getFontMetrics.stringWidth(xDataArg.get._4)).toInt, verticalTransformation(3))
    }
    if (yDataArg.isDefined) {
      drawRotate(graphics, graphics.getFontMetrics.getHeight-3, verticalTransformation(floor(pixelCanvasSize._2 * 0.5).toInt) + 0.5*graphics.getFontMetrics.stringWidth(yDataArg.get._4), 270, yDataArg.get._4)
    }
    graphics.setFont(currentFont.deriveFont(currentFont.getSize * 1.5F))

    // ticks specification
    if (xDataArg.isDefined) {
      val xTicks: NumericRange[Double] = xDataArg.get._1 to xDataArg.get._2 by xDataArg.get._3
      val posXTicks: IndexedSeq[(Int, Double)] = xTicks map mapHCoord  zip xTicks

      // draws x ticks
      posXTicks.foreach(tick => {
        graphics.drawLine(yBorderSpacing+tick._1, verticalTransformation(xBorderSpacing-5 + mapVCoord(origin._2)), yBorderSpacing+tick._1, verticalTransformation(xBorderSpacing+5 + mapVCoord(origin._2)))
        graphics.drawString(f"${tick._2}%1.2f", yBorderSpacing+tick._1-floor(0.5*graphics.getFontMetrics.stringWidth(f"${tick._2}%1.2f")).toInt, verticalTransformation(xBorderSpacing-5-2 + mapVCoord(origin._2)-graphics.getFontMetrics.getHeight))
      })
    }

    if (yDataArg.isDefined) {
      val yTicks: NumericRange[Double] = yDataArg.get._1 to yDataArg.get._2 by yDataArg.get._3
      val posYTicks: IndexedSeq[(Int, Double)] = yTicks map mapVCoord zip yTicks

      // draws y ticks
      posYTicks.foreach(tick => {
        graphics.drawLine(yBorderSpacing-5 + mapHCoord(origin._1), verticalTransformation(xBorderSpacing+tick._1), yBorderSpacing+5 + mapHCoord(origin._1), verticalTransformation(xBorderSpacing+tick._1))
        graphics.drawString(f"${tick._2}%1.2f", yBorderSpacing - 5 - 2 + mapHCoord(origin._1) - graphics.getFontMetrics.stringWidth(f"${tick._2}%1.2f"), verticalTransformation(xBorderSpacing+tick._1-floor(0.33*graphics.getFontMetrics.getHeight).toInt))
      })

    }
  }

  def drawScatterPoints(graphics: Graphics2D, x: Vector[Double], y: Vector[Double], color: Color = Color.RED): Unit = {
    graphics.setColor(color)
    val dotSize: Int = 10
    for (i <- x.indices) {
      graphics.fill(new Ellipse2D.Double(yBorderSpacing+mapHCoord(x(i)) - 0.5*dotSize, verticalTransformation(mapVCoord(y(i)))-xBorderSpacing-0.5*dotSize, dotSize, dotSize))
    }
  }

  def drawScatterPoints3D(graphics: Graphics2D, x: Vector[Double], y: Vector[Double], z: Vector[Double]): Unit = {
    val dotSize: Int = 10
    def colorMapFunction(x: Double): Color = colorMap(z.min, z.max)(x)
    val newZ: Vector[Double] = z.map(_-z.min)
    val minV: Double = newZ.min
    val maxV: Double = newZ.sorted.dropRight(2).max
    //val half: Double = minV + (maxV-minV)/2
    for (i <- z.indices) {
      val c: (Int, Int, Int) =
        //if (newZ(i) > half) { (((255 * (newZ(i) + min - half)) / half).toInt, (255-(255 * (newZ(i) + min - half)) / half).toInt, 0) }
        //else { (0, (255 - (255 * (half - newZ(i) + min)) / half).toInt, ((255 * (half - newZ(i) + min)) / half).toInt) }
        (min(255,(200 * newZ(i)/(maxV-minV)).toInt), min(255,(200 * newZ(i)/(maxV-minV)).toInt), min(255,(200 * newZ(i)/(maxV-minV)).toInt))
      println(colorMapFunction(z(i)), c)
      graphics.setColor(new Color(c._1, c._2, c._3))
      graphics.fill(new Ellipse2D.Double(yBorderSpacing+mapHCoord(x(i)) - 0.5*dotSize, verticalTransformation(mapVCoord(y(i)))-xBorderSpacing-0.5*dotSize, dotSize, dotSize))
    }
  }

  def drawLinearRegression(graphics: Graphics2D, xmax: Double, slope: Double, intercept: Double = 0.0, color: Color = Color.RED): Unit = {
    graphics.setColor(color)
    graphics.drawLine(yBorderSpacing+0, verticalTransformation(xBorderSpacing+mapVCoord(intercept)), yBorderSpacing + mapHCoord(xmax), verticalTransformation(xBorderSpacing + mapVCoord(intercept + xmax*slope)))
  }


  def drawHistogram(graphics: Graphics2D, binCount: Vector[(Int, Double)], intervals: Vector[Double], xLabel: String): Unit = {

    val barWidthPx: Int = (pixelCanvasSize._1 - yBorderSpacing*2)/(intervals.size-1)
    val barHeightPx: Int = mapVCoord(1.0)//scala.math.round((pixelCanvasSize._2-xBorderSpacing*2.toDouble)/(1.2*binCount.map(_._2).max)).toInt

    @tailrec
    def filterEveryOtherValue(x: Iterable[(Double, Double)], size: Int): Iterable[(Double, Double)] = {
      if (x.size <= size) { x }
      else { filterEveryOtherValue(x.zipWithIndex.filter(_._2 % 2 == 0).map(_._1), size) }
    }

    val intervals2Show: Iterable[(Double, Double)] = filterEveryOtherValue(intervals.dropRight(1).zip(intervals.tail), 6)

    intervals2Show.foreach(pair => {
      val str: String = "(" + "%1.0f".format(pair._1) + ", " + "%1.0f".format(pair._2) + "]"
      graphics.drawString(str, yBorderSpacing + mapHCoord(0.5*pair._1+0.5*pair._2)-floor(0.5*graphics.getFontMetrics.stringWidth(str)).toInt, verticalTransformation(xBorderSpacing-5-2 + mapVCoord(0.0)-graphics.getFontMetrics.getHeight))
    })

    graphics.setColor(Color.BLACK)
    graphics.drawString(xLabel, (0.5*pixelCanvasSize._1).toInt - (0.5*graphics.getFontMetrics.stringWidth(xLabel)).toInt, verticalTransformation(5))

    binCount.filter(_._1 >= 0).foreach(bar => {
      graphics.fillRect(barWidthPx/2+yBorderSpacing + mapHCoord(intervals(bar._1))-barWidthPx/2, verticalTransformation((bar._2*barHeightPx).toInt) - xBorderSpacing , barWidthPx, (bar._2*barHeightPx).toInt)
    })
  }

  def drawHeatMap(graphics: Graphics2D, data: Iterable[(Double, Double, Double)], xInterval: (Double, Int), yInterval: (Double, Int), minRange: Option[Double], maxRange: Option[Double]): Unit = {

    // define color map
    def colorMapFunction(x: Double): Color = colorMap(if (minRange.isDefined) minRange.get else data.map(_._3).min, if (maxRange.isDefined) maxRange.get else data.map(_._3).max)(x)

    // compute width and height of cells
    val cellWidthPx: Int = (pixelCanvasSize._1 - 2*yBorderSpacing)/xInterval._2
    val cellHeightPx: Int = (pixelCanvasSize._2 - 2*yBorderSpacing)/yInterval._2

/*    println(pixelCanvasSize)
    println(xInterval)
    println(yInterval)
    println(cellHeightPx, cellWidthPx)*/
    // draw all cells
    for (d <- data) {
      graphics.setColor(colorMapFunction(d._3))
     // println(d._1, d._2, d._3)
      //println(xBorderSpacing + mapVCoord(d._2), xBorderSpacing + mapVCoord(d._2)+(0.5*cellHeightPx).toInt, verticalTransformation(xBorderSpacing + mapVCoord(d._2)+(0.5*cellHeightPx).toInt), cellHeightPx)
      graphics.fillRect(yBorderSpacing + mapHCoord(d._1)-(0.5*cellWidthPx).toInt, verticalTransformation(xBorderSpacing + mapVCoord(d._2)+(0.5*cellHeightPx).toInt), cellWidthPx, cellHeightPx)
    }
  }

  def drawColorMapLegend(graphics: Graphics2D, valueMin: Double, valueMax: Double, label: String): Unit = {
    val cellWidth: Int = ((pixelCanvasSize._1 - 2*yBorderSpacing)*0.075).toInt
    val cellHeight: Int = ((pixelCanvasSize._2 - 2*yBorderSpacing)*0.075).toInt

    def colorMapFunction(x: Double): Color = colorMap(valueMin, valueMax)(x)

    graphics.setColor(Color.BLACK)
    if ((scala.BigDecimal(valueMax) to scala.BigDecimal(valueMin) by scala.BigDecimal(-(valueMax-valueMin)/10.0)).size == 10) {
      graphics.drawString(label, pixelCanvasSize._1 - 15, 2 * yBorderSpacing - (0.5 * cellHeight).toInt + 11 * cellHeight)
    } else {
      graphics.drawString(label, pixelCanvasSize._1 - 15, 2 * yBorderSpacing - (0.5 * cellHeight).toInt + 12 * cellHeight)

    }

    (valueMax to valueMin by -(valueMax-valueMin)/10.0).zipWithIndex.map(v => (v._2, v._1, colorMapFunction(v._1))).foreach(c => {
      graphics.setColor(Color.BLACK)
      graphics.drawString(f"${c._2}%1.2f", pixelCanvasSize._1 + cellWidth, 2*yBorderSpacing+c._1*cellHeight)
      graphics.setColor(c._3)
      graphics.fillRect(pixelCanvasSize._1-15, 2*yBorderSpacing-(0.5*cellHeight).toInt+c._1*cellHeight, cellWidth, cellHeight)
    })
  }

  def colorMap(minData: Double, maxData: Double)(value: Double): Color = {
    val x: Double = value - minData
    val minRange: Double = 0.0
    val maxRange:Double = maxData - minData
    new Color(math.max(0, math.min(255,(230 * x/(maxRange-minRange)).toInt)), math.max(0,math.min(255,(230 * x/(maxRange-minRange)).toInt)), math.max(0,math.min(255,(230 * x/(maxRange-minRange)).toInt)))
  }



}
