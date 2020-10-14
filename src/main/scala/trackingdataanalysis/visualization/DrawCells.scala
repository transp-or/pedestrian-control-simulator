package trackingdataanalysis.visualization

import java.awt.image.BufferedImage
import java.awt.{Color, Font, FontMetrics, Graphics2D}
import java.io.File

import javax.imageio.ImageIO
import kn.uni.voronoitreemap.j2d.Site

class DrawCells(cells: Seq[Site],
                filename: String,
                bkgdImage: Option[String],
                bkgdImageSizeMeters: (Double, Double)) extends VisualizationTools {

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
  def mapHcoord: Double => Int = mapHcoordLinear(0, bkgdImageSizeMeters._1, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapVcoordLinear(0, bkgdImageSizeMeters._2, canvasHeight)

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
    if (1000000.0 / h.getPolygon.getArea >= 2.17) gcleanCanvas.setColor(new Color(153, 0, 0, 25))
    else if (1000000.0 / h.getPolygon.getArea > 1.08) gcleanCanvas.setColor(new Color(255, 0, 0, 25))
    else if (1000000.0 / h.getPolygon.getArea > 0.72) gcleanCanvas.setColor(new Color(255, 128, 0, 25))
    else if (1000000.0 / h.getPolygon.getArea > 0.43) gcleanCanvas.setColor(new Color(255, 255, 0, 25))
    else if (1000000.0 / h.getPolygon.getArea > 0.31) gcleanCanvas.setColor(new Color(0, 255, 0, 25))
    else if (1000000.0 / h.getPolygon.getArea <= 0.31) gcleanCanvas.setColor(new Color(0, 0, 255, 25))
    gcleanCanvas.fillPolygon(h.getPolygon.getXPoints.take(h.getPolygon.getNumPoints).map(x => mapHcoord(x / 1000.0)), h.getPolygon.getYPoints.take(h.getPolygon.getNumPoints).map(y => mapVcoord(y / 1000.0)), h.getPolygon.getNumPoints)
    gcleanCanvas.setColor(Color.BLACK)
    gcleanCanvas.drawPolygon(h.getPolygon.getXPoints.take(h.getPolygon.getNumPoints).map(x => mapHcoord(x / 1000.0)), h.getPolygon.getYPoints.take(h.getPolygon.getNumPoints).map(y => mapVcoord(y / 1000.0)), h.getPolygon.getNumPoints)
    val w: Int = gcleanCanvas.getFontMetrics.stringWidth((1000.0 / h.getPolygon.getArea).toString) / 2
    /*if (w.toDouble > 0.85*mapHcoord(h.edgeLength)) {
      val currentFont = gcleanCanvas.getFont
      val newFont = currentFont.deriveFont(currentFont.getSize * 0.94.toFloat)
      gcleanCanvas.setFont(newFont)
    }*/
    gcleanCanvas.drawString((1.0 / h.getPolygon.getArea).toString, mapHcoord(h.x) - w, mapVcoord(h.y))
  })


  if (filename.length > 0) {
    ImageIO.write(cleanCanvas, filename.split("\\.").last, new java.io.File(filename))
  }
}