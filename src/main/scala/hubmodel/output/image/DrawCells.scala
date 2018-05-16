package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Font, FontMetrics, Graphics2D}
import javax.imageio.ImageIO

import hubmodel.output.video.Tools4Videos
import hubmodel.output._
import hubmodel.tools.cells.VertexPlotting

class DrawCells[T <: VertexPlotting](cells: Iterable[T],
                                     filename: String = "",
                                     mapFun: Option[(Double => Int, Double => Int)] = None,
                                     private val imHeight: Option[Int] = None
                                    ) extends Tools4Videos {

  val mappingFunctions: (Double => Int, Double => Int) = mapFun match {
    case None => {
      val bounds = getBoundsVertex(cells)
      computeMappingFunctions(bounds)
    }
    case s: Some[(Double => Int, Double => Int)] => s.get
  }

  val imageHeight: Int = imHeight match {
    case None => {
      val bounds = getBoundsVertex(cells)
      computeImageHeightPixels(bounds)
    }
    case s: Some[Int] => imHeight.get
  }

  if (filename.length > 0) {
    val image: BufferedImage = createWhiteBackgroundPixels((IMAGE_WIDTH + 20, imageHeight + 20))
    val verticalTransformation: Int => Int = verticalMirrorTransformation(image.getHeight)
    val gcleanCanvas: Graphics2D = image.createGraphics()
    gcleanCanvas.setColor(Color.BLACK)
    val currentFont: Font = gcleanCanvas.getFont
    //val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
    //gcleanCanvas.setFont(newFont)
    val metrics: FontMetrics = gcleanCanvas.getFontMetrics(currentFont)
    this.draw(gcleanCanvas, verticalTransformation)
    ImageIO.write(image, filename.split("\\.").last, new java.io.File(filename))
  }

  def draw(gImage: Graphics2D, verticalTransformation: Int => Int): Unit = {
    cells.foreach(h => {
      gImage.setColor(h.scalarToColor)
      gImage.fillPolygon(h.xCoords.map(mappingFunctions._1), h.yCoords.map(mappingFunctions._2).map(verticalTransformation), h.numberCorners)
      gImage.setColor(Color.BLACK)
      gImage.drawPolygon(h.xCoords.map(mappingFunctions._1), h.yCoords.map(mappingFunctions._2).map(verticalTransformation), h.numberCorners)
      val w: Int = gImage.getFontMetrics.stringWidth(h.stringToShow) / 2
      if (w.toDouble > 0.85 * mappingFunctions._1(h.horizontalMaxTextWidth)) {
        val currentFont = gImage.getFont
        val newFont = currentFont.deriveFont(currentFont.getSize * 0.94.toFloat)
        gImage.setFont(newFont)
      }
      gImage.drawString(h.stringToShow, mappingFunctions._1(h.center.X) - w, verticalTransformation(mappingFunctions._2(h.center.Y)))
    })
  }

}


