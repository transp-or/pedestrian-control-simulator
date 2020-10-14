package hubmodel.io.output.video

import java.awt.geom._
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D}
import java.io.File

import hubmodel.DES.PedestrianDES
import hubmodel._
import hubmodel.control.flowgate.BinaryGate
import hubmodel.io.output.createBackgroundFromImage
import hubmodel.ped.History.HistoryContainer
import hubmodel.ped.PedestrianSim
import org.jcodec.api.awt.AWTSequenceEncoder
import tools.Time
import tools.cells.Rectangle

/** Creates a video showing the movement of individual pedestrians with the critical zones in which the density is
  * monitored accompanied with the gates. Consider replacing the long list of arguments by the [[PedestrianDES]] object which
  * contains all the information needed for creating the video.
  *
  * The images are based on the times in the times2Show parameter. These times are used to build the timeEllipses
  * variable. One image is created for each value in the times2Show range.
  *
  * @param outputFile          name of the video
  * @param bkgdImage           url and name of the image to use as background
  * @param bkgdImageSizeMeters Tuple with the width and height in meters of the background image: (WIDTH, HEIGHT)
  * @param fps                 The number of frames per second
  * @param pop                 The set of pedestrians to draw
  * @param criticalAreaInput   The critical area in which the density is monitored
  * @param gateCollection      the collection of gates which are used to limit inflow into specific areas
  * @param gateHistory         the status over time of the gates (open or closed)
  * @param densityMeasurements Density values inside the critical zone.
  * @param times2Show          The set of times to show.
  */
@deprecated
class MovingPedestriansWithDensityVideo(outputFile: String,
                                        bkgdImage: Option[String],
                                        bkgdImageSizeMeters: (Double, Double),
                                        fps: Int,
                                        pop: Vector[PedestrianSim],
                                        criticalAreaInput: List[Rectangle],
                                        gateCollection: Map[String, BinaryGate],
                                        var gateHistory: scala.collection.mutable.ArrayBuffer[(Int, List[(String, Boolean)])],
                                        var densityMeasurements: collection.mutable.ArrayBuffer[(Int, Double)],
                                        times2Show: Vector[Double]) extends Tools4Videos {

  assert(times2Show.size > 0.0)
  println(" * writing " + times2Show.size + " frames at " + fps + " frames per second.")

  // Loads the background image which is used as the base image for all dimensions.

  val cleanCanvas: BufferedImage = createBackgroundFromImage(bkgdImage, bkgdImageSizeMeters)


  val canvasWidth: Int = if (cleanCanvas.getWidth % 2 == 0) cleanCanvas.getWidth else cleanCanvas.getWidth + 1
  val canvasHeight: Int = if (cleanCanvas.getHeight % 2 == 0) cleanCanvas.getHeight else cleanCanvas.getHeight + 1

  val dotSize: Double = 0.35 * canvasWidth.toDouble / bkgdImageSizeMeters._1

  //val cleanCanvas: BufferedImage = ImageIO.read(new File(bkgdImage))
  //new BufferedImage(canvasWidth + 1, canvasHeight + 1, BufferedImage.TYPE_4BYTE_ABGR)

  /** Function which returns an [[Ellipse2D]] to draw at a specific location
    *
    * @return Function taing a [[Position]] and returning an [[Ellipse2D]] representing a pedestrian at a specific location
    */
  def createDot: Position => Ellipse2D = createDot((mapHcoord, mapVcoord), dotSize)

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapCoordLinear(bkgdImageSizeMeters._1, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapCoordLinear(bkgdImageSizeMeters._2, canvasHeight)

  // formatting of the data: aggregation by times of the positions.
  val population2TimePositionList: List[(Time, List[HistoryContainer])] = mergeListsByTime(pop.flatMap(_.getHistoryPosition).toList).sortBy(_._1)
  // List of times with corresponding ellipses to draw. For each time, the list of ellipses coreespond to the pedestrians.

  //println(population2TimePositionList.map(_._1).toVector.sorted)
  var timeEllipses: Vector[(Time, List[Ellipse2D])] = population2TimePositionList.filter(pair => times2Show.exists(t => (t - pair._1.value).abs.toDouble <= math.pow(10, -5))).map(p => (p._1, p._2.map(a => createDot(a.pos)))).toVector
  //println(population2TimePositionList.filter(pair => times2Show.contains(pair._1)).map(_._1).sorted)

  // Image to use for combining all the different components: the bkgd image, the dots, the monitored areas, the gates, etc.
  val combine: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)

  // Encoder for creating the video from BufferedImages.
  val enc: AWTSequenceEncoder = AWTSequenceEncoder.createSequenceEncoder(new File(outputFile), fps)
  //enc.getEncoder.setKeyInterval(fps)

  // Main iteration creating images, then pushing them to the encoder.
  //for (i <- timeEllipses.indices) {
  var counter: Int = 0
  for (i <- times2Show.indices) {
    //if (100*times2Show(i)/(times2Show.end-times2Show.head).toInt % 5 == 0) {println(100*times2Show(i)/(times2Show.end-times2Show.head))}

    /** Draws dots representing pedestrians */
    val points: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    //if (timeEllipses.head._1 == times2Show(i)) {
    val gPoints: Graphics2D = points.createGraphics()
    gPoints.setColor(Color.RED)
    timeEllipses.indexWhere(te => (times2Show(i) - te._1.value).abs.toDouble < math.pow(10, -5)) match {
      case a if a >= 0 => timeEllipses(a)._2.foreach(p => gPoints.fill(p))
      case _ => {}
    }
    //timeEllipses = timeEllipses.tail
    //}

    /** Draws colored boxes showing densities inside areas */

    val box: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (densityMeasurements.nonEmpty && criticalAreaInput.nonEmpty) {
      val criticalArea: Rectangle = criticalAreaInput.head
      val gBox: Graphics2D = box.createGraphics()
      if (densityMeasurements.head._2 >= 2.17) gBox.setColor(new Color(153, 0, 0, 25))
      else if (densityMeasurements.head._2 > 1.08) gBox.setColor(new Color(255, 0, 0, 25))
      else if (densityMeasurements.head._2 > 0.72) gBox.setColor(new Color(255, 128, 0, 25))
      else if (densityMeasurements.head._2 > 0.43) gBox.setColor(new Color(255, 255, 0, 25))
      else if (densityMeasurements.head._2 > 0.31) gBox.setColor(new Color(0, 255, 0, 25))
      else if (densityMeasurements.head._2 <= 0.31) gBox.setColor(new Color(0, 0, 255, 25))
      val areaCoords: (Position, Position, Position, Position) = (criticalArea.corners(0), criticalArea.corners(1), criticalArea.corners(2), criticalArea.corners(3))
      gBox.fillRect(mapHcoord(areaCoords._1.X), mapVcoord(areaCoords._1.Y), mapHcoord(areaCoords._3.X) - mapHcoord(areaCoords._4.X), mapVcoord(areaCoords._4.Y) - mapVcoord(areaCoords._1.Y))
      gBox.setColor(Color.BLACK)
      gBox.drawRect(mapHcoord(areaCoords._1.X), mapVcoord(areaCoords._1.Y), mapHcoord(areaCoords._3.X) - mapHcoord(areaCoords._4.X), mapVcoord(areaCoords._4.Y) - mapVcoord(areaCoords._1.Y))
    }

    /** Draws gate states */
    val gates: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (gateHistory.nonEmpty) {
      val gGates: Graphics2D = gates.createGraphics()
      gGates.setStroke(new BasicStroke(7))
      gGates.setColor(Color.RED)
      gateHistory.head._2
        .filter(!_._2)
        .map(og => (gateCollection(og._1).start, gateCollection(og._1).end))
        .foreach(coords => gGates.drawLine(mapHcoord(coords._1.X), mapVcoord(coords._1.Y), mapHcoord(coords._2.X), mapVcoord(coords._2.Y)))
    }

    /** combine images into one */
    def timeReadable(t: Double): String = {
      val hours: Int = scala.math.floor(t / 3600.0).toInt
      val minutes: Int = scala.math.floor((t - hours * 3600) / 60.0).toInt
      val seconds: Double = t - hours * 3600 - minutes * 60
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    val gCombine: Graphics2D = combine.createGraphics()
    gCombine.drawImage(cleanCanvas, 0, 0, null)
    if (densityMeasurements.nonEmpty) {
      gCombine.drawImage(box, 0, 0, null)
    }

    gCombine.drawImage(points, 0, 0, null)
    gCombine.drawImage(gates, 0, 0, null)
    gCombine.setColor(Color.BLACK)
    gCombine.drawString(timeReadable(times2Show(i)), 200, 50)
    if (densityMeasurements.nonEmpty) {
      gCombine.drawString(densityMeasurements.head._2.toString, 200, 80)
    }

    // Removes the head of the history containers when the time of the current images reaches the time of the head of
    // the history containers.
    if (densityMeasurements.size > 1 && densityMeasurements.tail.head._1 == times2Show(i)) {
      densityMeasurements = densityMeasurements.tail
    }

    if (gateHistory.nonEmpty && gateHistory.tail.head._1 == times2Show(i)) {
      gateHistory = gateHistory.tail
    }

    // pushes current image to encoder to make video
    enc.encodeImage(combine)

    counter += 1
    if (counter % 100 == 0) {
      System.out.print("\r * " + counter + "/" + times2Show.size + " frames encoded")
    }

  }

  // closes encoder
  enc.finish()
}
