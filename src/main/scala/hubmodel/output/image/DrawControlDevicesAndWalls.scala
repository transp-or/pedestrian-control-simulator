package hubmodel.output.image

import java.awt.geom.AffineTransform
import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import hubmodel.mgmt.ControlDevices
import hubmodel.output._
import hubmodel.supply.continuous.Wall
import myscala.math.vector.Vector2D

class DrawControlDevicesAndWalls(filename: String = "",
                                 walls: Iterable[Wall],
                                 devices: ControlDevices) {

  val wallBounds: (Double, Double, Double, Double) = getBounds(walls)
  val IMAGE_HEIGHT: Int = computeImageHeightPixels(wallBounds)

  val mappingFunctions: (Double => Int, Double => Int) = (
    mapCoordAffine(wallBounds._1, wallBounds._3, IMAGE_WIDTH),
    mapCoordAffine(wallBounds._2, wallBounds._4, IMAGE_HEIGHT)
  )

  val devicesImage: BufferedImage = createWhiteBackgroundPixels((IMAGE_WIDTH + 20, IMAGE_HEIGHT + 20))

  val gDevices: Graphics2D = devicesImage.createGraphics()
  gDevices.setColor(Color.BLACK)

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

  def draw(gImage: Graphics2D, verticalTransformation: Int => Int): Unit = {
    devices.flowSeparators.foreach(fs => {
      (fs.inflowLinesStart ++ fs.inflowLinesEnd).foreach(fl => {

        // draws line
        gImage.drawLine(
          mappingFunctions._1(fl.start.X),
          verticalTransformation(mappingFunctions._2(fl.start.Y)),
          mappingFunctions._1(fl.end.X),
          verticalTransformation(mappingFunctions._2(fl.end.Y))
        )

        // draws near region
        gImage.drawPolygon(fl.nearRegion.corners.map(_.X).map(mappingFunctions._1).toArray, fl.nearRegion.corners.map(_.Y).map(mappingFunctions._2).map(verticalTransformation).toArray, 4)
        println(fl.nearRegion.corners.map(_.X))
        println(fl.nearRegion.corners.map(_.Y))
        println(fl.nearRegion.corners.map(_.X).map(mappingFunctions._1).toArray.mkString("\t"))
        println(fl.nearRegion.corners.map(_.Y).map(mappingFunctions._2).toArray.mkString("\t"))

        // draws inflow direction
        val inflowDirStart: Vector2D = fl.start + (fl.end-fl.start)*0.5 - (fl.end-fl.start).orthogonal*2.0*0.5
        val inflowDirEnd: Vector2D = fl.start + (fl.end-fl.start)*0.5 + (fl.end-fl.start).orthogonal*2.0*0.5
        gImage.drawLine(
          mappingFunctions._1(inflowDirStart.X),
          verticalTransformation(mappingFunctions._2(inflowDirStart.Y)),
          mappingFunctions._1(inflowDirEnd.X),
          verticalTransformation(mappingFunctions._2(inflowDirEnd.Y))
        )
        drawArrow(gImage, mappingFunctions._1(inflowDirStart.X), verticalTransformation(mappingFunctions._2(inflowDirStart.Y)), mappingFunctions._1(inflowDirEnd.X), verticalTransformation(mappingFunctions._2(inflowDirEnd.Y)))
      })
    })
  }

  if (filename.length > 0) {
    val verticalTransformation: Int => Int = verticalMirrorTransformation(devicesImage.getHeight)
    this.draw(gDevices, verticalTransformation)
    val wallImage = new DrawWalls(walls, mapFun = Some(mappingFunctions), imHeight = Some(IMAGE_HEIGHT))
    wallImage.draw(gDevices, verticalTransformation)
    ImageIO.write(devicesImage, filename.split("\\.").last, new java.io.File(filename))
  }

}
