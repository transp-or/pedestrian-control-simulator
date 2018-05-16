package hubmodel

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File
import javax.imageio.ImageIO

import breeze.linalg.{max, min}
import breeze.numerics.round
import hubmodel.supply.continuous.Wall
import hubmodel.tools.cells.{Rectangle, Vertex, VertexPlotting}

/**
  * Created by nicholas on 6/11/17.
  */
package object output {

  // Thickness of the border around the main drawing.
  val border: Int = 5

  val IMAGE_WIDTH: Int = 2000 + 2 * border

  def computeMappingFunctions(bounds: (Double, Double, Double, Double)): (Double => Int, Double => Int) = {
    val minX = bounds._1
    val minY = bounds._2
    val maxX = bounds._3
    val maxY = bounds._4
    val hRange: Double = maxX - minX
    val vRange: Double = maxY - minY

    val widthPixel: Int = IMAGE_WIDTH
    val heightPixel: Int = {
      if ((round(vRange / hRange * widthPixel).toInt % 2) == 0) {
        round(vRange / hRange * widthPixel).toInt
      }
      else {
        round(vRange / hRange * widthPixel).toInt + 1
      }
    }

    val hMapping: Double => Int = coordX => 10 + round(((coordX - minX) / hRange) * widthPixel).toInt
    val vMapping: Double => Int = coordY => 10 + round(((coordY - minY) / vRange) * heightPixel).toInt
    (hMapping, vMapping)
  }

  def computeImageHeightPixels(bounds: (Double, Double, Double, Double)): Int = {
    val minX = bounds._1
    val minY = bounds._2
    val maxX = bounds._3
    val maxY = bounds._4
    val hRange: Double = maxX - minX
    val vRange: Double = maxY - minY
    if ((round(vRange / hRange * IMAGE_WIDTH).toInt % 2) == 0) {
      border * 2 + round(vRange / hRange * IMAGE_WIDTH).toInt
    }
    else {
      border * 2 + round(vRange / hRange * IMAGE_WIDTH).toInt + 1
    }
  }

  def verticalMirrorTransformation(imHeight: Int)(yCoord: Int): Int = {
    -yCoord + imHeight
  }

  def getBounds(walls: Iterable[Wall]): (Double, Double, Double, Double) = {
    val minX: Double = walls.map(w => min(w.startPoint.X, w.endPoint.X)).min
    val maxX: Double = walls.map(w => max(w.startPoint.X, w.endPoint.X)).max
    val minY: Double = walls.map(w => min(w.startPoint.Y, w.endPoint.Y)).min
    val maxY: Double = walls.map(w => max(w.startPoint.Y, w.endPoint.Y)).max
    (minX, minY, maxX, maxY)
  }

  def getBounds(edges: Vector[(Vertex, Vertex)]): (Double, Double, Double, Double) = {
    val minX: Double = min(edges.map(e => min(min(e._1.corners.map(_.X)), min(e._1.corners.map(_.X)))))
    val minY: Double = min(edges.map(e => min(min(e._1.corners.map(_.Y)), min(e._1.corners.map(_.Y)))))
    val maxX: Double = max(edges.map(e => max(max(e._1.corners.map(_.X)), max(e._1.corners.map(_.X)))))
    val maxY: Double = max(edges.map(e => max(max(e._1.corners.map(_.Y)), max(e._1.corners.map(_.Y)))))
    (minX, minY, maxX, maxY)
  }

  def getBoundsVertex(cells: Iterable[Vertex]): (Double, Double, Double, Double) = {
    val xMin: Double = cells.flatMap(_.corners.map(_.X)).min
    val xMax: Double = cells.flatMap(_.corners.map(_.X)).max
    val yMin: Double = cells.flatMap(_.corners.map(_.Y)).min
    val yMax: Double = cells.flatMap(_.corners.map(_.Y)).max
    (xMin, yMin, xMax, yMax)
  }

  /** Mapping function for horizontal (width) coordinates. Transforms a coordinate into pixels.
    * The pixelWidth argumet is the width of the full canvas. The border is removed inside the function.
    *
    * @param trueSize  height in meters of the image
    * @param pixelSize height in pixels of the image
    * @param coord     point to map
    * @return the position in pixels of the original coordinate
    */
  def mapCoordAffine(trueMin: Double, trueMax: Double, pixelWidth: Int)(coord: Double): Int = {
    math.round((coord - trueMin) * ((pixelWidth - 2 * border) / (trueMax - trueMin)) + border).toInt
  }

  def createWhiteBackground(bkgdImageSizeMeters: (Double, Double)): BufferedImage = {

    val initialWidth: Int = bkgdImageSizeMeters._1.ceil.toInt * 20
    val initialHeight: Int = bkgdImageSizeMeters._2.ceil.toInt * 20

    // rounds the canvas width to an even number
    val canvasWidth: Int = if (initialWidth % 2 == 0) initialWidth else initialWidth + 1
    val canvasHeight: Int = if (initialHeight % 2 == 0) initialHeight else initialHeight + 1
    val canv: BufferedImage = new BufferedImage(border * 2 + canvasWidth, border * 2 + canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val gcanv: Graphics2D = canv.createGraphics()
    gcanv.setColor(Color.WHITE)
    gcanv.fillRect(0, 0, border * 2 + canvasWidth, border * 2 + canvasHeight)
    canv
  }

  def createWhiteBackgroundPixels(bkgdImageSizeMeters: (Double, Double)): BufferedImage = {

    val initialWidth: Int = bkgdImageSizeMeters._1.ceil.toInt
    val initialHeight: Int = bkgdImageSizeMeters._2.ceil.toInt

    // rounds the canvas width to an even number
    val canvasWidth: Int = if (initialWidth % 2 == 0) initialWidth else initialWidth + 1
    val canvasHeight: Int = if (initialHeight % 2 == 0) initialHeight else initialHeight + 1
    val canv: BufferedImage = new BufferedImage(border * 2 + canvasWidth, border * 2 + canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val gcanv: Graphics2D = canv.createGraphics()
    gcanv.setColor(Color.WHITE)
    gcanv.fillRect(0, 0, border * 2 + canvasWidth, border * 2 + canvasHeight)
    canv
  }

  def createBackgroundFromImage(bkgdImage: Option[String], bkgdImageSizeMeters: (Double, Double)): BufferedImage = {
    bkgdImage match {
      case Some(f) => try {
        ImageIO.read(new File(f))
      } catch {
        case ime: javax.imageio.IIOException => println(ime + " in MakeVideo for file: " + f); createWhiteBackground(bkgdImageSizeMeters)
        case e: Throwable => println(e + " in MakeVideo for file: " + f); createWhiteBackground(bkgdImageSizeMeters)
      }
      case None => {
        createWhiteBackground(bkgdImageSizeMeters)
      }
    }
  }
}
