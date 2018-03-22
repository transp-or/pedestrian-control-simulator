package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Graphics2D}
import javax.imageio.ImageIO

import hubmodel.output._
import hubmodel.supply.continuous.Wall

class DrawWalls(walls: Vector[Wall],
                filename: String = "",
                mapFun: Option[(Double => Int, Double => Int)] = None,
                private val imHeight: Option[Int] = None,
                showNames: Boolean = false) {

  val mappingFunctions: (Double => Int, Double => Int) = mapFun match {
    case None => {
      val bounds = getBounds(walls)
      computeMappingFunctions(bounds)
    }
    case s: Some[(Double => Int, Double => Int)] => s.get
  }

  val imageHeight: Int = imHeight match {
    case None => {
      val bounds = getBounds(walls)
      computeImageHeightPixels(bounds)
    }
    case s: Some[Int] => imHeight.get
  }

  if (filename.length > 0) {
    val image = new BufferedImage(IMAGE_WIDTH + 20, imageHeight + 20, BufferedImage.TYPE_4BYTE_ABGR)
    val verticalTransformation: Int => Int = verticalMirrorTransformation(image.getHeight)
    val gImage: Graphics2D = image.createGraphics()
    gImage.setColor(Color.WHITE)
    gImage.fillRect(0, 0, image.getWidth(), image.getHeight())
    gImage.setColor(Color.BLACK)
    if (showNames) drawComments(gImage, verticalTransformation)
    draw(gImage, verticalTransformation)
    ImageIO.write(image, filename.split("\\.").last, new java.io.File(filename))
  }

  def draw(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    walls.foreach(w => gImage.drawLine(
      mappingFunctions._1(w.startPoint.X),
      verticalMirrorTransformation(mappingFunctions._2(w.startPoint.Y)),
      mappingFunctions._1(w.endPoint.X),
      verticalMirrorTransformation(mappingFunctions._2(w.endPoint.Y)))
    )
  }

  def drawComments(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    val currentFont: Font = gImage.getFont
    val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
    gImage.setFont(newFont)
    walls.foreach(w => gImage.drawString(
      w.comment,
      (mappingFunctions._1(w.startPoint.X) + mappingFunctions._1(w.endPoint.X)) / 2,
      verticalMirrorTransformation((mappingFunctions._2(w.startPoint.Y) + mappingFunctions._2(w.endPoint.Y)) / 2)))
  }

}