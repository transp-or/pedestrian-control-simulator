package hubmodel

import breeze.numerics.round

/**
  * Created by nicholas on 6/11/17.
  */
package object output {

  val IMAGE_WIDTH: Int = 2000

  def computeMappingFunctions(bounds: Vector[Double]): (Double => Int, Double => Int) = {
    val minX = bounds(0)
    val minY = bounds(1)
    val maxX = bounds(2)
    val maxY = bounds(3)
    val hRange: Double =  maxX - minX
    val vRange: Double =  maxY - minY

    val widthPixel: Int = IMAGE_WIDTH
    val heightPixel: Int = {
      if ((round(vRange/hRange * widthPixel).toInt % 2) == 0) {round(vRange/hRange * widthPixel).toInt}
      else {round(vRange/hRange * widthPixel).toInt + 1}
    }

    val hMapping: Double => Int = coordX => 10 + round(( (coordX - minX) / hRange) * widthPixel).toInt
    val vMapping: Double => Int = coordY => 10 + round(( (coordY - minY) / vRange) * heightPixel).toInt
    (hMapping, vMapping)
  }

  def computeImageHeight(bounds: Vector[Double]): Int = {
    val minX = bounds(0)
    val minY = bounds(1)
    val maxX = bounds(2)
    val maxY = bounds(3)
    val hRange: Double =  maxX - minX
    val vRange: Double =  maxY - minY
    if ((round(vRange/hRange * IMAGE_WIDTH).toInt % 2) == 0) {round(vRange/hRange * IMAGE_WIDTH).toInt}
    else {round(vRange/hRange * IMAGE_WIDTH).toInt + 1}
  }

  def verticalMirrorTransformation(imHeight: Int)(yCoord: Int): Int = {
    -yCoord + imHeight
  }
}
