package trackingdataanalysis.visualization

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File

import breeze.linalg.DenseVector
import breeze.numerics.{abs, floor}
import javax.imageio.ImageIO
import kn.uni.voronoitreemap.j2d.Site
import org.jcodec.api.awt.AWTSequenceEncoder
import trackingdataanalysis.pedtrack.Pedestrian

import scala.collection.immutable.NumericRange

class MovingPedestriansWithVoronoiDensity(outputFile: String,
                                          bkgdImage: Option[String],
                                          bkgdImageSizeMeters: (Double, Double),
                                          fps: Int,
                                          cells: Vector[(Double, Seq[Site])],
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
    pop(id).h_t.indexWhere(t => abs(t - l._1) < 0.05) match {
      case i if i >= 0 => DenseVector(pop(id).h_x(i), pop(id).h_y(i))
    }
  }).toVector)).sortWith(_._1 < _._1)
  //MergeListsByTime(pop.flatMap(_.getHistoryPosition).toList, times2Show).sortWith(_._1 < _._1)
  //println(population2TimePositionList.map(_._1).mkString("\n"))
  //println(population2TimePositionList.mkString("\n"))
  // List of times with corresponding ellipses to draw. For each time, the list of ellipses coreespond to the pedestrians.
  //println(times2Show)
  //println(population2TimePositionList.map(_._1).toVector.sorted)
  var timeEllipses: Vector[(Double, Vector[Ellipse2D])] = population2TimePositionList.map(p => (p._1, p._2.map(createDot))).filter(pair => times2Show.contains(pair._1))
  //println(population2TimePositionList.map(p => (p._1, p._2.map(createDot))).map(_._1))
  // Image to use for combining all the different components: the bkgd image, the dots, the monitored areas, the gates, etc.
  val combine: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)

  // Encoder for creating the video from BufferedImages.
  val enc: AWTSequenceEncoder = AWTSequenceEncoder.createSequenceEncoder(new File(outputFile), fps)
  //enc.getEncoder.setKeyInterval(fps)

  //println(timeEllipses.map(_._1).zip(cells.map(_._1)).mkString("\n"))

  // Main iteration creating images, then pushing them to the encoder.
  for (i <- timeEllipses.indices) {
    // println(timeEllipses.head)
    //println(cells(i))

    //if (100*times2Show(i)/(times2Show.end-times2Show.head).toInt % 5 == 0) {println(100*times2Show(i)/(times2Show.end-times2Show.head))}

    /** Draws dots representing pedestrians */
    val points: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val gPoints: Graphics2D = points.createGraphics()
    gPoints.setColor(Color.RED)
    timeEllipses.head._2 foreach (p => gPoints.fill(p))
    timeEllipses = timeEllipses.tail

    val voronoi: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    //val gVoronoi: Graphics2D = voronoi.createGraphics()
    cells(i)._2.foreach(h => {
      if (1.0 / h.getPolygon.getArea >= 2.17) gPoints.setColor(new Color(153, 0, 0, 25))
      else if (1.0 / h.getPolygon.getArea > 1.08) gPoints.setColor(new Color(255, 0, 0, 25))
      else if (1.0 / h.getPolygon.getArea > 0.72) gPoints.setColor(new Color(255, 128, 0, 25))
      else if (1.0 / h.getPolygon.getArea > 0.43) gPoints.setColor(new Color(255, 255, 0, 25))
      else if (1.0 / h.getPolygon.getArea > 0.31) gPoints.setColor(new Color(0, 255, 0, 25))
      else if (1.0 / h.getPolygon.getArea <= 0.31) gPoints.setColor(new Color(0, 0, 255, 25))
      gPoints.fillPolygon(h.getPolygon.getXPoints.take(h.getPolygon.getNumPoints).map(x => mapHcoord(x)), h.getPolygon.getYPoints.take(h.getPolygon.getNumPoints).map(y => mapVcoord(y)), h.getPolygon.getNumPoints)
      gPoints.setColor(Color.BLACK)
      gPoints.fillOval(mapHcoord(h.x), mapVcoord(h.y), 2, 2)
      gPoints.drawPolygon(h.getPolygon.getXPoints.take(h.getPolygon.getNumPoints).map(x => mapHcoord(x)), h.getPolygon.getYPoints.take(h.getPolygon.getNumPoints).map(y => mapVcoord(y)), h.getPolygon.getNumPoints)
      val w: Int = gPoints.getFontMetrics.stringWidth((1.0 / h.getPolygon.getArea).toString) / 2
      /*if (w.toDouble > 0.85*mapHcoord(h.edgeLength)) {
        val currentFont = gcleanCanvas.getFont
        val newFont = currentFont.deriveFont(currentFont.getSize * 0.94.toFloat)
        gcleanCanvas.setFont(newFont)
      }*/

      //gPoints.drawString((1.0 / h.getPolygon.getArea).toString, mapHcoord(h.x) - w, mapVcoord(h.y))
    })

    /** Draws colored boxes showing densities inside areas */
    /*val box: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (densityMeasurements.nonEmpty) {
      val gBox: Graphics2D = box.createGraphics()
      if (densityMeasurements.head._2 >= 2.17) gBox.setColor(new Color(153, 0, 0, 25))
      else if (densityMeasurements.head._2 > 1.08) gBox.setColor(new Color(255, 0, 0, 25))
      else if (densityMeasurements.head._2 > 0.72) gBox.setColor(new Color(255, 128, 0, 25))
      else if (densityMeasurements.head._2 > 0.43) gBox.setColor(new Color(255, 255, 0, 25))
      else if (densityMeasurements.head._2 > 0.31) gBox.setColor(new Color(0, 255, 0, 25))
      else if (densityMeasurements.head._2 <= 0.31) gBox.setColor(new Color(0, 0, 255, 25))
      gBox.fillRect(mapHcoord(criticalArea.A(0)), mapVcoord(criticalArea.A(1)), mapHcoord(criticalArea.C(0)) - mapHcoord(criticalArea.D(0)), mapVcoord(criticalArea.D(1)) - mapVcoord(criticalArea.A(1)))
      gBox.setColor(Color.BLACK)
      gBox.drawRect(mapHcoord(criticalArea.A(0)), mapVcoord(criticalArea.A(1)), mapHcoord(criticalArea.C(0)) - mapHcoord(criticalArea.D(0)), mapVcoord(criticalArea.D(1)) - mapVcoord(criticalArea.A(1)))
    }*/
    /** Draws gate states */
    /*val gates: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (gateHistory.nonEmpty) {
      val gGates: Graphics2D = gates.createGraphics()
      gGates.setStroke(new BasicStroke(7))
      gGates.setColor(Color.RED)
      gateHistory.head._2
        .filter(!_._2)
        .map(og => (gateCollection(og._1).startPos, gateCollection(og._1).endPos))
        .foreach(coords => gGates.drawLine(mapHcoord(coords._1(0)), mapVcoord(coords._1(1)), mapHcoord(coords._2(0)), mapVcoord(coords._2(1))))
    }*/

    /** combine images into one */
    def timeReadable(t: Int): String = {
      val hours: Int = floor(t / 3600000.0).toInt
      val minutes: Int = floor((t - hours * 3600000) / 60000.0).toInt
      val seconds: Double = t - hours * 3600000 - minutes * 60000
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    val gCombine: Graphics2D = combine.createGraphics()
    gCombine.drawImage(cleanCanvas, 0, 0, null)
    //if (densityMeasurements.nonEmpty) { gCombine.drawImage(box, 0, 0, null) }
    gCombine.drawImage(points, 0, 0, null)
    //gCombine.drawImage(voronoi, 0, 0, null)
    gCombine.setColor(Color.BLACK)
    gCombine.drawString(times2Show(i).toString, 200, 50)
    /*if (densityMeasurements.nonEmpty) { gCombine.drawString(densityMeasurements.head._2.toString, 200, 80) }

    // Removes the head of the history containers when the time of the current images reaches the time of the head of
    // the history containers.
    if (densityMeasurements.nonEmpty && densityMeasurements.tail.head._1 == times2Show(i)) {
      densityMeasurements = densityMeasurements.tail
    }

    if (gateHistory.nonEmpty && gateHistory.tail.head._1 == times2Show(i)) {
      gateHistory = gateHistory.tail
    }*/

    // pushes current image to encoder to make video
    enc.encodeImage(combine)
  }

  // closes encoder
  enc.finish()

}