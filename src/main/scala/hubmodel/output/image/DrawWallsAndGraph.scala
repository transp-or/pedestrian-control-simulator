package hubmodel.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import javax.imageio.ImageIO

import hubmodel.output._
import hubmodel.supply.continuous.Wall
import hubmodel.tools.cells.RectangularVertexTrait

class DrawWallsAndGraph(walls: Vector[Wall], edges: Vector[(RectangularVertexTrait, RectangularVertexTrait)], filename: String) {

  val wallBounds: (Double, Double, Double, Double) = getBounds(walls)
  val graphBounds: (Double, Double, Double, Double) = getBounds(edges)
  val trueXMin: Double = math.min(wallBounds._1, graphBounds._1)
  val trueXMax: Double = math.max(wallBounds._3, graphBounds._3)
  val trueYMin: Double = math.min(wallBounds._2, graphBounds._2)
  val trueYMax: Double = math.max(wallBounds._4, graphBounds._4)

  val IMAGE_HEIGHT: Int = computeImageHeightPixels((trueXMin, trueYMin, trueXMax, trueYMax))

  val mappingFunctions: (Double => Int, Double => Int) = (
    mapCoordAffine(trueXMin, trueXMax, IMAGE_WIDTH),
    mapCoordAffine(trueYMin, trueYMax, IMAGE_HEIGHT)
  )

  val wallImage = new DrawWalls(walls, mapFun = Some(mappingFunctions), imHeight = Some(IMAGE_HEIGHT))
  val graphImage = new DrawGraph(edges, mapFun = Some(mappingFunctions), imHeight = Some(IMAGE_HEIGHT))

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