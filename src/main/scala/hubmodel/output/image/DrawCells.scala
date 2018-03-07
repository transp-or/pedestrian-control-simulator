package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Graphics2D}
import java.io.File
import javax.imageio.ImageIO

import hubmodel.output.video.Tools4Videos
import hubmodel.route.MyCell

class DrawCells(cells: IndexedSeq[MyCell],
                filename: String,
                bkgdImage: Option[String],
                bkgdImageSizeMeters: (Double, Double)) extends Tools4Videos {

  val cleanCanvas: BufferedImage = bkgdImage match {
    case Some(f) => ImageIO.read(new File(f))
    case None => {
      val canv: BufferedImage = new BufferedImage(bkgdImageSizeMeters._1.round.toInt * 30, bkgdImageSizeMeters._2.round.toInt * 30, BufferedImage.TYPE_4BYTE_ABGR)
      val gcanv: Graphics2D = canv.createGraphics()
      gcanv.setColor(Color.WHITE)
      gcanv.fillRect(0, 0, bkgdImageSizeMeters._1.round.toInt * 30, bkgdImageSizeMeters._2.round.toInt * 30)
      canv
    }
  }

  val canvasWidth: Int = if (cleanCanvas.getWidth % 2 == 0) cleanCanvas.getWidth else cleanCanvas.getWidth + 1
  val canvasHeight: Int = if (cleanCanvas.getHeight % 2 == 0) cleanCanvas.getHeight else cleanCanvas.getHeight + 1

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapCoordLinear(bkgdImageSizeMeters._1, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapCoordLinear(bkgdImageSizeMeters._2, canvasHeight)

  val gcleanCanvas: Graphics2D = cleanCanvas.createGraphics()
  gcleanCanvas.setColor(Color.BLACK)

  val currentFont: Font = gcleanCanvas.getFont
  //val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
  //gcleanCanvas.setFont(newFont)

  import java.awt.FontMetrics

  val metrics: FontMetrics = gcleanCanvas.getFontMetrics(currentFont)
  // Determine the X coordinate for the text
  // Determine the Y coordinate for the text (note we add the ascent, as in java 2d 0 is top of the screen)
  // Set the font
  // Draw the String

  cells.foreach(h => {
    if (h.pedAcc / h.area >= 2.17) gcleanCanvas.setColor(new Color(153, 0, 0, 25))
    else if (h.pedAcc / h.area > 1.08) gcleanCanvas.setColor(new Color(255, 0, 0, 25))
    else if (h.pedAcc / h.area > 0.72) gcleanCanvas.setColor(new Color(255, 128, 0, 25))
    else if (h.pedAcc / h.area > 0.43) gcleanCanvas.setColor(new Color(255, 255, 0, 25))
    else if (h.pedAcc / h.area > 0.31) gcleanCanvas.setColor(new Color(0, 255, 0, 25))
    else if (h.pedAcc / h.area <= 0.31) gcleanCanvas.setColor(new Color(0, 0, 255, 25))
    gcleanCanvas.fillPolygon(h.xCoords.map(mapHcoord), h.yCoords.map(mapVcoord), 6)
    gcleanCanvas.setColor(Color.BLACK)
    gcleanCanvas.drawPolygon(h.xCoords.map(mapHcoord), h.yCoords.map(mapVcoord), 6)
    val w: Int = gcleanCanvas.getFontMetrics.stringWidth(h.pedAcc.toString + " / " + "%1.2f".format(h.potential)) / 2
    if (w.toDouble > 0.85 * mapHcoord(h.edgeLength)) {
      val currentFont = gcleanCanvas.getFont
      val newFont = currentFont.deriveFont(currentFont.getSize * 0.94.toFloat)
      gcleanCanvas.setFont(newFont)
    }
    gcleanCanvas.drawString(h.pedAcc.toString + " / " + "%1.2f".format(h.potential), mapHcoord(h.center(0)) - w, mapVcoord(h.center(1)))
  })


  if (filename.length > 0) {
    ImageIO.write(cleanCanvas, filename.split("\\.").last, new java.io.File(filename))
  }
}


