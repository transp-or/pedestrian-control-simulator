package hubmodel.output

import java.awt.geom.Rectangle2D
import java.awt.{Color, Font, Graphics2D}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import breeze.linalg.{max, min}
import breeze.numerics.round
import hubmodel.{Position, VertexCell}
import hubmodel.input.infrastructure.{Wall}
import hubmodel.route.MyCell
import java.awt.Font


/**
  * Created by nicholas on 6/9/17.
  */
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
      mappingFunctions._1(w.x1),
      verticalMirrorTransformation(mappingFunctions._2(w.y1)),
      mappingFunctions._1(w.x2),
      verticalMirrorTransformation(mappingFunctions._2(w.y2)))
    )
  }

  def drawComments(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    val currentFont: Font = gImage.getFont
    val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
    gImage.setFont(newFont)
    walls.foreach(w => gImage.drawString(
      w.comment,
      (mappingFunctions._1(w.x1) + mappingFunctions._1(w.x2)) / 2,
      verticalMirrorTransformation((mappingFunctions._2(w.y1) + mappingFunctions._2(w.y2)) / 2)))
  }

  def getBounds(walls: Vector[Wall]): Vector[Double] = {
    val minX: Double = min(walls.map(w => min(w.x1, w.x2)))
    val maxX: Double = max(walls.map(w => max(w.x1, w.x2)))
    val minY: Double = min(walls.map(w => min(w.y1, w.y2)))
    val maxY: Double = max(walls.map(w => max(w.y1, w.y2)))
    Vector(minX, minY, maxX, maxY)
  }
}


class DrawGraph(edges: Vector[(VertexCell, VertexCell)],
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
    val xPoints = Array(mapFun._1(A(0)), mapFun._1(B(0)), mapFun._1(C(0)), mapFun._1(D(0)))
    val yPoints = Array(mapFun._2(A(1)), mapFun._2(B(1)), mapFun._2(C(1)), mapFun._2(D(1)))
    (xPoints, yPoints, 4)
  }

  def draw(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    val currentFont: Font = gImage.getFont
    val newFont: Font = currentFont.deriveFont(currentFont.getSize * 1.5F)
    gImage.setFont(newFont)
    val polygons: Vector[(Array[Int], Array[Int], Int)] = edges.flatMap(e => List(e._1, e._2)).distinct.map(v => StructurePoints4Polygon(v.A, v.B, v.C, v.D, mappingFunctions))
    polygons.foreach(p => gImage.drawPolygon(p._1, p._2.map(verticalMirrorTransformation), p._3))
    edges.foreach(e => gImage.drawLine(mappingFunctions._1(e._1.center(0)), verticalMirrorTransformation(mappingFunctions._2(e._1.center(1))), mappingFunctions._1(e._2.center(0)), verticalMirrorTransformation(mappingFunctions._2(e._2.center(1)))))
  }

  def drawNames(gImage: Graphics2D, verticalMirrorTransformation: Int => Int): Unit = {
    edges.flatMap(e => List(e._1, e._2)).distinct.foreach(v => gImage.drawString(v.name, mappingFunctions._1(v.center(0)), verticalMirrorTransformation(mappingFunctions._2(v.center(1)))))
  }

  def getBounds(edges: Vector[(VertexCell, VertexCell)]): Vector[Double] = {
    val minX: Double = min(edges.map(e => min(min(e._1.A(0), e._1.B(0)), min(e._1.C(0), e._1.D(0)))))
    val minY: Double = min(edges.map(e => min(min(e._1.A(1), e._1.B(1)), min(e._1.C(1), e._1.D(1)))))
    val maxX: Double = max(edges.map(e => max(max(e._1.A(0), e._1.B(0)), max(e._1.C(0), e._1.D(0)))))
    val maxY: Double = max(edges.map(e => max(max(e._1.A(1), e._1.B(1)), max(e._1.C(1), e._1.D(1)))))
    Vector(minX, minY, maxX, maxY)
  }
}


class DrawWallsAndGraph(walls: Vector[Wall], edges: Vector[(VertexCell, VertexCell)], filename: String) {
  val wallImage = new DrawWalls(walls)
  val graphImage = new DrawGraph(edges, mapFun = Some(wallImage.mappingFunctions), imHeight = Some(wallImage.imageHeight))

  val image: BufferedImage = new BufferedImage(IMAGE_WIDTH, wallImage.imageHeight, BufferedImage.TYPE_4BYTE_ABGR)
  val verticalTransformation: Int => Int = verticalMirrorTransformation(image.getHeight)

  val gImage: Graphics2D = image.createGraphics()
  gImage.setColor(Color.WHITE)
  gImage.fillRect(0, 0, image.getWidth(), image.getHeight())
  gImage.setColor(Color.BLACK)
  wallImage.draw(gImage, verticalTransformation)
  gImage.setColor(Color.RED)
  graphImage.draw(gImage, verticalTransformation)
  graphImage.drawNames(gImage, verticalTransformation)
  ImageIO.write(image, filename.split("\\.").last, new java.io.File(filename))
}

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
  def mapHcoord: Double => Int = mapHcoordLinear(bkgdImageSizeMeters._1, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapVcoordLinear(bkgdImageSizeMeters._2, canvasHeight)

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


