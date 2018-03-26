package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Font, FontMetrics, Graphics2D}
import javax.imageio.ImageIO

import hubmodel.output.{createBackgroundFromImage, mapCoordAffine}
import hubmodel.tools.cells.{NewVertexPlotting}

class DrawCellsOverImage[T <: NewVertexPlotting](
                          bkgdImage: Option[String],
                          bkgdImageSizeMeters: (Double, Double, Double, Double),
                          cells: Iterable[T],
                          fileName: String
                        ) {

  val trueWidth: Double = bkgdImageSizeMeters._3 - bkgdImageSizeMeters._1
  val trueHeight: Double = bkgdImageSizeMeters._4 - bkgdImageSizeMeters._2


  val cleanCanvas: BufferedImage = createBackgroundFromImage(bkgdImage, (trueWidth, trueHeight))

  val canvasWidth: Int = cleanCanvas.getWidth
  val canvasHeight: Int = cleanCanvas.getHeight

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapCoordAffine(bkgdImageSizeMeters._1, bkgdImageSizeMeters._3, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapCoordAffine(bkgdImageSizeMeters._2, bkgdImageSizeMeters._4,canvasHeight)


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
    gcleanCanvas.fillPolygon(h.xCoords.map(mapHcoord), h.yCoords.map(mapVcoord), h.numberCorners)
    gcleanCanvas.setColor(Color.BLACK)
    gcleanCanvas.drawPolygon(h.xCoords.map(mapHcoord), h.yCoords.map(mapVcoord), h.numberCorners)
    val w: Int = gcleanCanvas.getFontMetrics.stringWidth(h.stringToShow) / 2
    if (w.toDouble > 0.85 * mapHcoord(h.horizontalMaxTextWidth)) {
      val currentFont = gcleanCanvas.getFont
      val newFont = currentFont.deriveFont(currentFont.getSize * 0.94.toFloat)
      gcleanCanvas.setFont(newFont)
    }
    gcleanCanvas.drawString(h.stringToShow, mapHcoord(h.center.X) - w, mapVcoord(h.center.Y))
  })


  if (fileName.length > 0) {
    ImageIO.write(cleanCanvas, fileName.split("\\.").last, new java.io.File(fileName))
  }

}
