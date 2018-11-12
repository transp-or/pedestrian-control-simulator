package trackingdataanalysis.visualization

import java.awt.Graphics2D
import java.awt.geom.Ellipse2D

import breeze.linalg.DenseVector

import scala.annotation.tailrec
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.math.BigDecimal.RoundingMode

trait VisualizationTools {

  type Time = Double
  type Position = DenseVector[Double]

  /** Combines a List of (Time, Position) tuples into a List of (Time, List[Position]) tuples.
    * This makes drawing process easier.
    *
    * @param hist original data formatted as List[(Time, Position)]
    * @return reformating of data as List[(Time, List[Position])]
    */
  def MergeListsByTime(hist: List[(Time, Position)]): List[(Int, List[Position])] = {
    hist.groupBy(_._1).map { case (k, v) => ((k * 1).round.toInt, v.map(_._2)) }.toList
  }

  def MergeListsByTime(hist: List[(Time, Position)], times: Seq[Double]): List[(Double, List[Position])] = {
    hist.groupBy(_._1).filter(t => times.contains(t._1)).map { case (k, v) => (k, v.map(_._2)) }.toList
  }


  /** Template function for creating dots representing pedestrians.
    * The first group of parameters are the sizes in pixels and meters for the mapping functions.
    * The second group of parameters is only composed of one parameters, and is simply the position
    * at which to draw the dot.
    *
    * @param hMeters height in meters of the image
    * @param hPixels height in pixels of the image
    * @param wMeters width in meters of the image
    * @param wPixels width in pixels of the image
    * @param pos     position to draw the dot
    * @return an ellipse2D object to draw using the fill method from[[Graphics2D]]
    */
  def createDot(hMeters: Double, hPixels: Int, wMeters: Double, wPixels: Int, size: Double)(pos: Position): Ellipse2D = {
    new Ellipse2D.Double(pos(0) / wMeters * wPixels - 0.5*size, pos(1) / hMeters * hPixels - 0.5*size, size, size)
  }

  /** Mapping function for horizontal (width) coordinates
    *
    * @param hMeters height in meters of the image
    * @param hPixels height in pixels of the image
    * @param hPos    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapHcoordLinear(trueMin: Double, trueMax: Double, pixelWidth: Int)(x: Double): Int = math.round((x-trueMin) / (trueMax-trueMin) * pixelWidth).toInt

  /** Mapping function for vertical (height) coordinates
    *
    * @param trueWidth width in meters of the image
    * @param pixelWidth width in pixels of the image
    * @param wPos    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapVcoordLinear(trueMin: Double, trueMax: Double, pixelHeight: Int)(y: Double): Int = math.round((y-trueMin) / (trueMax-trueMin) * pixelHeight).toInt

  def mapHcoordAffine(trueMin: Double, trueMax: Double, pixelMin: Int, pixelMax: Int)(x: Double): Int = pixelMin + math.round((x-trueMin) / (trueMax-trueMin) * (pixelMax-pixelMin)).toInt


  def mapHcoordLinearBD(trueMin: BigDecimal, trueMax: BigDecimal, pixelWidth: Int)(x: BigDecimal): Int = ((x-trueMin) / (trueMax-trueMin) * pixelWidth).setScale(0, RoundingMode.HALF_UP).intValue()//new MathContext(4, RoundingMode.HALF_UP))

  /** Mapping function for vertical (height) coordinates
    *
    * @param trueWidth width in meters of the image
    * @param pixelWidth width in pixels of the image
    * @param wPos    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapVcoordLinearBD(trueMin: BigDecimal, trueMax: BigDecimal, pixelHeight: Int)(y: BigDecimal): Int = ((y-trueMin) / (trueMax-trueMin) * pixelHeight).setScale(0, RoundingMode.HALF_UP).intValue()

  def mapHcoordAffineBD(trueMin: BigDecimal, trueMax: BigDecimal, pixelMin: Int, pixelMax: Int)(x: BigDecimal): Int = pixelMin + ((x-trueMin) / (trueMax-trueMin) * (pixelMax-pixelMin)).setScale(0, RoundingMode.HALF_UP).intValue()



  //def mapVcoordAffine(wTrue: Double, lowerBound: Double, wPixels: Int)(wPos: Double): Int = floor((wPos - lowerBound) / (wTrue - lowerBound) * wPixels).toInt

  def drawRotate(graphics: Graphics2D, x: Double, y: Double, angle: Int, text: String): Unit = {
    graphics.translate(x,y)
    graphics.rotate(Math.toRadians(angle))
    graphics.drawString(text,0,0)
    graphics.rotate(-Math.toRadians(angle))
    graphics.translate(-x,-y)
  }

  def verticalMirrorTransformation(imHeight: Int)(yCoord: Int): Int = {
    -yCoord + imHeight
  }

  @tailrec
  final def filterEveryOtherValue[T](x: Seq[T], size: Int)/*(implicit cbf: CanBuildFrom[T, Double, T])*/: Seq[T]  = {
    if (x.size <= size) { x }
    else {
      filterEveryOtherValue(x.zipWithIndex.filter(_._2 % 2 == 0).map(_._1), size)
    }
  }

 /* def filterEveryOtherValue2[U, A <: Seq[U]](x: A, s: Int)(implicit bf: CanBuildFrom[A, U, A]): A = {
    if (x.size == s) { x }
    else { filterEveryOtherValue2(x.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).asInstanceOf[A], s) }
  }*/


  def trimNonWordCharacters[T <: Iterable[String]](items: T with IterableLike[String, T])(implicit cbf: CanBuildFrom[T, String, T]): T =
    items map { _.replaceAll("\\W", "") }

}
