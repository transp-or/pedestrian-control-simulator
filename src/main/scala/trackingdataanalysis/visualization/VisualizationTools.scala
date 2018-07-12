package trackingdataanalysis.visualization

import java.awt.Graphics2D
import java.awt.geom.Ellipse2D

import breeze.linalg.DenseVector
import breeze.numerics.round

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
  def mapHcoordLinear(trueMin: Double, trueMax: Double, pixelWidth: Int)(x: Double): Int = round((x-trueMin) / (trueMax-trueMin) * pixelWidth).toInt

  /** Mapping function for vertical (height) coordinates
    *
    * @param trueWidth width in meters of the image
    * @param pixelWidth width in pixels of the image
    * @param wPos    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapVcoordLinear(trueMin: Double, trueMax: Double, pixelHeight: Int)(y: Double): Int = round((y-trueMin) / (trueMax-trueMin) * pixelHeight).toInt

  //def mapHcoordAffine(hTrue: Double, lowerBound: Double, hPixels: Int)(hPos: Double): Int = floor((hPos - lowerBound) / (hTrue - lowerBound) * hPixels).toInt

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
}
