package hubmodel.io.output.image

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import hubmodel.io.output._
import hubmodel.supply.continuous.Wall
import hubmodel.tools.cells.VertexPlotting
import javax.imageio.ImageIO

class DrawCellsAndWalls[T <: VertexPlotting](cells: Iterable[T], walls: Iterable[Wall], fileName: String) {

  val wallBounds: (Double, Double, Double, Double) = getBounds(walls)

  val IMAGE_HEIGHT: Int = computeImageHeightPixels(wallBounds)

  val mappingFunctions: (Double => Int, Double => Int) = (
    mapCoordAffine(wallBounds._1, wallBounds._3, IMAGE_WIDTH),
    mapCoordAffine(wallBounds._2, wallBounds._4, IMAGE_HEIGHT)
  )

  val wallImage = new DrawWalls(walls, mapFun = Some(mappingFunctions), imHeight = Some(IMAGE_HEIGHT))
  val cellImage = new DrawCells[T](cells, mapFun = Some(mappingFunctions), imHeight = Some(IMAGE_HEIGHT))

  val image: BufferedImage = new BufferedImage(IMAGE_WIDTH, wallImage.imageHeight, BufferedImage.TYPE_4BYTE_ABGR)
  val verticalTransformation: Int => Int = verticalMirrorTransformation(image.getHeight)

  val gImage: Graphics2D = image.createGraphics()
  gImage.setColor(Color.WHITE)
  gImage.fillRect(0, 0, image.getWidth(), image.getHeight())
  gImage.setColor(Color.BLACK)
  wallImage.draw(gImage, verticalTransformation)
  gImage.setColor(Color.RED)
  cellImage.draw(gImage, verticalTransformation)
  ImageIO.write(image, fileName.split("\\.").last, new java.io.File(fileName))
}
