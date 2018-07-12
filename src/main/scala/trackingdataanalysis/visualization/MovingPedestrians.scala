package trackingdataanalysis.visualization

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File

import breeze.linalg.DenseVector
import breeze.numerics.{abs, floor}
import javax.imageio.ImageIO
import org.jcodec.api.awt.AWTSequenceEncoder
import trackingdataanalysis.pedtrack.Pedestrian

import scala.collection.immutable.NumericRange

class MovingPedestrians(outputFile: String,
                        bkgdImage: Option[String],
                        bkgdImageSizeMeters: (Double, Double),
                        fps: Int,
                        pop: Map[Int, Pedestrian],
                        timeMap: Vector[(Double, Iterable[Int])],
                        times2Show: NumericRange[Double]) extends VisualizationTools {

  println("Starting processing video...")


  // Loads the background image which is used as the base image for all dimensions.
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

  val dotSize: Double = 0.35 * canvasWidth.toDouble / bkgdImageSizeMeters._1

  //val cleanCanvas: BufferedImage = ImageIO.read(new File(bkgdImage))
  //new BufferedImage(canvasWidth + 1, canvasHeight + 1, BufferedImage.TYPE_4BYTE_ABGR)

  /** Function which returns an [[Ellipse2D]] to draw at a specific location
    *
    * @return Function taing a [[Position]] and returning an [[Ellipse2D]] representing a pedestrian at a specific location
    */
  def createDot: Position => Ellipse2D = createDot(bkgdImageSizeMeters._2, canvasHeight, bkgdImageSizeMeters._1, canvasWidth, dotSize)

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

  // formatting of the data: aggregation by times of the positions.
  val population2TimePositionList: Vector[(Double, Vector[Position])] = timeMap.map(l => (l._1, l._2.map(id => {
    pop(id).h_t.indexWhere(t => abs(t - l._1) < 0.005) match {
      case i if i >= 0 => DenseVector(pop(id).h_x(i), pop(id).h_y(i))
    }
  }).toVector)).sortWith(_._1 < _._1)


  var timeEllipses: Vector[(Double, Vector[Ellipse2D])] = population2TimePositionList.map(p => (p._1, p._2.map(createDot))).filter(pair => times2Show.contains(pair._1))

  // Image to use for combining all the different components: the bkgd image, the dots, the monitored areas, the gates, etc.
  val combine: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)

  // Encoder for creating the video from BufferedImages.
  val enc: AWTSequenceEncoder = AWTSequenceEncoder.createSequenceEncoder(new File(outputFile), fps)


  // Main iteration creating images, then pushing them to the encoder.
  for (i <- timeEllipses.indices) {

    /** Draws dots representing pedestrians */
    val points: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val gPoints: Graphics2D = points.createGraphics()
    gPoints.setColor(Color.RED)
    timeEllipses.head._2 foreach (p => gPoints.fill(p))
    timeEllipses = timeEllipses.tail

    /** combine images into one */
    def timeReadable(t: Int): String = {
      val hours: Int = floor(t / 3600000.0).toInt
      val minutes: Int = floor((t - hours * 3600000) / 60000.0).toInt
      val seconds: Double = t - hours * 3600000 - minutes * 60000
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    val gCombine: Graphics2D = combine.createGraphics()
    gCombine.drawImage(cleanCanvas, 0, 0, null)
    gCombine.drawImage(points, 0, 0, null)
    gCombine.setColor(Color.BLACK)
    gCombine.drawString(times2Show(i).toString, 200, 50)


    // pushes current image to encoder to make video
    enc.encodeImage(combine)
  }

  // closes encoder
  enc.finish()

}