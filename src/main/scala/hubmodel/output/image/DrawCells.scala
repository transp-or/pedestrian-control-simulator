package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Font, FontMetrics, Graphics2D}
import javax.imageio.ImageIO

import hubmodel.output.video.Tools4Videos
import hubmodel.output.{createWhiteBackground, mapCoordAffine}
import hubmodel.tools.MyCellTrait

class DrawCells[T <: MyCellTrait](cells: Iterable[T], filename: String) extends Tools4Videos {

  val xMin: Double = cells.flatMap(_.xCoords).min
  val xMax: Double = cells.flatMap(_.xCoords).max
  val yMin: Double = cells.flatMap(_.yCoords).min
  val yMax: Double = cells.flatMap(_.yCoords).max


  val cleanCanvas: BufferedImage = createWhiteBackground((xMax-xMin, yMax - yMin))

  //val IMAGE_HEIGHT: Int = computeImageHeightPixels(xMin, yMin, xMax, yMax)

  val canvasWidth: Int = cleanCanvas.getWidth
  val canvasHeight: Int = cleanCanvas.getHeight

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapCoordAffine(xMin, xMax, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapCoordAffine(yMin, yMax,canvasHeight)

  val gcleanCanvas: Graphics2D = cleanCanvas.createGraphics()
  gcleanCanvas.setColor(Color.BLACK)

  val currentFont: Font = gcleanCanvas.getFont
  //val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
  //gcleanCanvas.setFont(newFont)


  val metrics: FontMetrics = gcleanCanvas.getFontMetrics(currentFont)
  // Determine the X coordinate for the text
  // Determine the Y coordinate for the text (note we add the ascent, as in java 2d 0 is top of the screen)
  // Set the font
  // Draw the String

  cells.foreach(h => {
    gcleanCanvas.setColor(h.scalarToColor)
    gcleanCanvas.fillPolygon(h.xCoords.map(mapHcoord), h.yCoords.map(mapVcoord), h.numberAngles)
    gcleanCanvas.setColor(Color.BLACK)
    gcleanCanvas.drawPolygon(h.xCoords.map(mapHcoord), h.yCoords.map(mapVcoord), h.numberAngles)
    val w: Int = gcleanCanvas.getFontMetrics.stringWidth(h.stringToShow) / 2
    if (w.toDouble > 0.85 * mapHcoord(h.horizontalMaxTextWidth)) {
      val currentFont = gcleanCanvas.getFont
      val newFont = currentFont.deriveFont(currentFont.getSize * 0.94.toFloat)
      gcleanCanvas.setFont(newFont)
    }
    gcleanCanvas.drawString(h.stringToShow, mapHcoord(h.center.X) - w, mapVcoord(h.center.Y))
  })


  if (filename.length > 0) {
    ImageIO.write(cleanCanvas, filename.split("\\.").last, new java.io.File(filename))
  }
}


