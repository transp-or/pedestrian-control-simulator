package hubmodel

import breeze.linalg.{max, min}
import breeze.numerics.round
import hubmodel.supply.Wall

/**
  * Created by nicholas on 6/11/17.
  */
package object output {

  // Thickness of the border around the main drawing.
  val border: Int = 5

  val IMAGE_WIDTH: Int = 2000 + 2*border

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
      border*2 + round(vRange / hRange * IMAGE_WIDTH).toInt
    }
    else {
      border*2 + round(vRange / hRange * IMAGE_WIDTH).toInt + 1
    }
  }

  def verticalMirrorTransformation(imHeight: Int)(yCoord: Int): Int = {
    -yCoord + imHeight
  }

  def getBounds(walls: Iterable[Wall]): (Double, Double, Double, Double) = {
    val minX: Double = walls.map(w => min(w.x1, w.x2)).min
    val maxX: Double = walls.map(w => max(w.x1, w.x2)).max
    val minY: Double = walls.map(w => min(w.y1, w.y2)).min
    val maxY: Double = walls.map(w => max(w.y1, w.y2)).max
    (minX, minY, maxX, maxY)
  }

  def getBounds(edges: Vector[(VertexRectangle, VertexRectangle)]): (Double, Double, Double, Double) = {
    val minX: Double = min(edges.map(e => min(min(e._1.A.X, e._1.B.X), min(e._1.C.X, e._1.D.X))))
    val minY: Double = min(edges.map(e => min(min(e._1.A.Y, e._1.B.Y), min(e._1.C.Y, e._1.D.Y))))
    val maxX: Double = max(edges.map(e => max(max(e._1.A.X, e._1.B.X), max(e._1.C.X, e._1.D.X))))
    val maxY: Double = max(edges.map(e => max(max(e._1.A.Y, e._1.B.Y), max(e._1.C.Y, e._1.D.Y))))
    (minX, minY, maxX, maxY)
  }

  /** Mapping function for horizontal (width) coordinates. Transforms a coordinate into pixels.
    * The pixelWidth argumet is the width of the full canvas. The border is removed inside the function.
    *
    * @param trueSize height in meters of the image
    * @param pixelSize height in pixels of the image
    * @param coord    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapCoordAffine(trueMin: Double, trueMax: Double, pixelWidth: Int)(coord: Double): Int = {
    math.round((coord-trueMin)*((pixelWidth-2*border)/(trueMax-trueMin)) + border).toInt
  }
}
