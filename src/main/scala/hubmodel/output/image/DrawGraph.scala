package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Graphics2D}

import hubmodel.Position
import hubmodel.output._
import hubmodel.tools.cells.Rectangle
import javax.imageio.ImageIO

class DrawGraph(edges: Vector[(Rectangle, Rectangle)],
                filename: String = "",
                mapFun: Option[(Double => Int, Double => Int)] = None,
                imHeight: Option[Int] = None) {

  val mappingFunctions: (Double => Int, Double => Int) = mapFun match {
    case None => {
      val bounds = getBounds(edges)
      computeMappingFunctions(bounds)
    }
    case s: Some[(Double => Int, Double => Int)] => s.get
  }

  val imageHeight: Int = imHeight match {
    case None => {
      val bounds = getBounds(edges)
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
    draw(gImage, verticalTransformation)
    drawNames(gImage, verticalTransformation)
    ImageIO.write(image, filename.split("\\.").last, new java.io.File(filename))
  }

  def StructurePoints4Polygon(A: Position, B: Position, C: Position, D: Position, mapFun: (Double => Int, Double => Int)): (Array[Int], Array[Int], Int) = {
    val xPoints = Array(mapFun._1(A.X), mapFun._1(B.X), mapFun._1(C.X), mapFun._1(D.X))
    val yPoints = Array(mapFun._2(A.Y), mapFun._2(B.Y), mapFun._2(C.Y), mapFun._2(D.Y))
    (xPoints, yPoints, 4)
  }

  def StructurePoints4Polygon(corners: Vector[Position], mapFun: (Double => Int, Double => Int)): (Array[Int], Array[Int], Int) = {
    StructurePoints4Polygon(corners(0), corners(1), corners(2), corners(3), mapFun)
  }

  def draw(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    val currentFont: Font = gImage.getFont
    val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
    gImage.setFont(newFont)
    val polygons: Vector[(Array[Int], Array[Int], Int)] = edges.flatMap(e => List(e._1, e._2)).distinct.map(v => StructurePoints4Polygon(v.corners, mappingFunctions))
    polygons.foreach(p => gImage.drawPolygon(p._1, p._2.map(verticalMirrorTransformation), p._3))
    edges.foreach(e => gImage.drawLine(mappingFunctions._1(e._1.center.X), verticalMirrorTransformation(mappingFunctions._2(e._1.center.Y)), mappingFunctions._1(e._2.center.X), verticalMirrorTransformation(mappingFunctions._2(e._2.center.Y))))
    edges.foreach(e => drawArrow(gImage, mappingFunctions._1(e._1.center.X), verticalMirrorTransformation(mappingFunctions._2(e._1.center.Y)), mappingFunctions._1(e._2.center.X), verticalMirrorTransformation(mappingFunctions._2(e._2.center.Y))))
  }

  def drawNames(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    edges.flatMap(e => List(e._1, e._2)).distinct.foreach(v => gImage.drawString(v.name, mappingFunctions._1(v.center.X), verticalMirrorTransformation(mappingFunctions._2(v.center.Y))))
  }

  import java.awt.Graphics2D
  import java.awt.geom.AffineTransform

  def drawArrow(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    val dx = x2 - x1
    val dy = y2 - y1
    val angle = Math.atan2(dy, dx)
    val len = Math.sqrt(dx * dx + dy * dy).toInt
    val at = AffineTransform.getTranslateInstance(x1, y1)
    at.concatenate(AffineTransform.getRotateInstance(angle))
    g.transform(at)

    // Draw horizontal arrow starting in (0, 0)
    g.fillPolygon(Array[Int]((0.25*len).toInt, (0.25*len).toInt - 5, (0.25*len).toInt - 5, (0.25*len).toInt), Array[Int](0, -5, 5, 0), 4)
    g.transform(at.createInverse())
  }

}