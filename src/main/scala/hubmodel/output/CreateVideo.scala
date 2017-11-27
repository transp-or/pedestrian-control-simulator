package hubmodel.output

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D}
import java.awt.geom._
import java.io.File
import javax.imageio.ImageIO

import breeze.numerics.floor
import hubmodel.input.infrastructure.BinaryGate
import hubmodel.{PedestrianDES, PedestrianSim, Position, Time, Vertex, timePrint, timeReadable}
import org.jcodec.api.awt.AWTSequenceEncoder

import scala.collection.immutable.NumericRange

/**
  * Collection of various tools for formatting data, drawing elements and mapping coordinates.
  */
trait Tools4Videos {

  /** Combines a List of (Time, Position) tuples into a List of (Time, List[Position]) tuples.
    * This makes drawing process easier.
    *
    * @param hist original data formatted as List[(Time, Position)]
    * @return reformating of data as List[(Time, List[Position])]
    */
  def MergeListsByTime(hist: List[(Time, Position)]): List[(Int, List[Position])] = {
    hist.groupBy(_._1).map { case (k, v) => ((k*1000).round.toInt, v.map(_._2)) }.toList
  }

  /** Template function for creating dots representing pedestrians.
    * The first group of parameters are the sizes in pixels and meters for the mapping functions.
    * The second group of parameters is only composed of one parameters, and is simply the position
    * at which to draw the dot.
    *
    * @param hMeters height in meters of the image
    * @param hPixels height in pixels of the image
    * @param wMeters width in meters of the image
    * @param wPixels width in pixels of the image
    * @param pos     position to draw the dot
    * @return an ellipse2D object to draw using the fill method from[[Graphics2D]]
    */
  def createDot(hMeters: Double, hPixels: Int, wMeters: Double, wPixels: Int, size: Double)(pos: Position): Ellipse2D = {
    new Ellipse2D.Double(pos(0) / wMeters * wPixels, pos(1) / hMeters * hPixels, size, size)
  }

  /** Mapping function for horizontal (width) coordinates
    *
    * @param hMeters height in meters of the image
    * @param hPixels height in pixels of the image
    * @param hPos    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapHcoordLinear(hMeters: Double, hPixels: Int)(hPos: Double): Int = floor(hPos / hMeters * hPixels).toInt

  /** Mapping function for vertical (height) coordinates
    *
    * @param wMeters width in meters of the image
    * @param wPixels width in pixels of the image
    * @param wPos    point to map
    * @return the position in pixels of the original coordinate
    */
  def mapVcoordLinear(wMeters: Double, wPixels: Int)(wPos: Double): Int = floor(wPos / wMeters * wPixels).toInt

  def mapHcoordAffine(hTrue: Double, lowerBound: Double, hPixels: Int)(hPos: Double): Int = floor((hPos-lowerBound) / (hTrue-lowerBound) * hPixels).toInt
  def mapVcoordAffine(wTrue: Double, lowerBound: Double, wPixels: Int)(wPos: Double): Int = floor((wPos-lowerBound) / (wTrue-lowerBound) * wPixels).toInt


}

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
  * @param criticalArea        The critical area in which the density is monitored
  * @param gateCollection      the collection of gates which are used to limit inflow into specific areas
  * @param gateHistory         the status over time of the gates (open or closed)
  * @param densityMeasurements Density values inside the critical zone.
  * @param times2Show          The set of times to show.
  */
class MovingPedestriansWithDensityVideo(outputFile: String,
                                        bkgdImage: Option[String],
                                        bkgdImageSizeMeters: (Double, Double),
                                        fps: Int,
                                        pop: Vector[PedestrianSim],
                                        criticalArea: Vertex,
                                        gateCollection: Map[String, BinaryGate],
                                        var gateHistory: scala.collection.mutable.ArrayBuffer[(Int, List[(String, Boolean)])],
                                        var densityMeasurements: collection.mutable.ArrayBuffer[(Int, Double)],
                                        times2Show: Range) extends Tools4Videos {


  // Loads the background image which is used as the base image for all dimensions.
  val cleanCanvas: BufferedImage = bkgdImage match {
    case Some(f) => ImageIO.read(new File(f))
    case None => {
      val canv: BufferedImage = new BufferedImage(bkgdImageSizeMeters._1.round.toInt*30, bkgdImageSizeMeters._2.round.toInt*30, BufferedImage.TYPE_4BYTE_ABGR)
      val gcanv: Graphics2D = canv.createGraphics()
      gcanv.setColor(Color.WHITE)
      gcanv.fillRect(0, 0, bkgdImageSizeMeters._1.round.toInt*30, bkgdImageSizeMeters._2.round.toInt*30)
      canv
    }
  }

  val canvasWidth: Int = if (cleanCanvas.getWidth % 2 == 0) cleanCanvas.getWidth else cleanCanvas.getWidth+1
  val canvasHeight: Int = if (cleanCanvas.getHeight % 2 == 0) cleanCanvas.getHeight else cleanCanvas.getHeight+1

  val dotSize: Double = 0.35*canvasWidth.toDouble/bkgdImageSizeMeters._1

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
  def mapHcoord: Double => Int = mapHcoordLinear(bkgdImageSizeMeters._1, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapVcoordLinear(bkgdImageSizeMeters._2, canvasHeight)

  // formatting of the data: aggregation by times of the positions.
  val population2TimePositionList: List[(Int, List[Position])] = MergeListsByTime(pop.flatMap(_.getHistoryPosition).toList).sortWith(_._1 < _._1)
  // List of times with corresponding ellipses to draw. For each time, the list of ellipses coreespond to the pedestrians.
  //println(times2Show)
  //println(population2TimePositionList.map(_._1).toVector.sorted)
  var timeEllipses: List[(Int, List[Ellipse2D])] = population2TimePositionList.map(p => (p._1, p._2.map(createDot))).filter(pair => times2Show.contains(pair._1))
  //println(population2TimePositionList.map(p => (p._1, p._2.map(createDot))).map(_._1))

  // Image to use for combining all the different components: the bkgd image, the dots, the monitored areas, the gates, etc.
  val combine: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)

  // Encoder for creating the video from BufferedImages.
  val enc: AWTSequenceEncoder = AWTSequenceEncoder.createSequenceEncoder(new File(outputFile), fps)
  //enc.getEncoder.setKeyInterval(fps)

  // Main iteration creating images, then pushing them to the encoder.
  for (i <- timeEllipses.indices) {

    //if (100*times2Show(i)/(times2Show.end-times2Show.head).toInt % 5 == 0) {println(100*times2Show(i)/(times2Show.end-times2Show.head))}

    /** Draws dots representing pedestrians */
    val points: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val gPoints: Graphics2D = points.createGraphics()
    gPoints.setColor(Color.RED)
    timeEllipses.head._2 foreach (p => gPoints.fill(p))
    timeEllipses = timeEllipses.tail

    /** Draws colored boxes showing densities inside areas */
    val box: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
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
    }
    /** Draws gate states */
    val gates: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (gateHistory.nonEmpty) {
        val gGates: Graphics2D = gates.createGraphics()
        gGates.setStroke(new BasicStroke(7))
        gGates.setColor(Color.RED)
        gateHistory.head._2
          .filter(!_._2)
          .map(og => (gateCollection(og._1).startPos, gateCollection(og._1).endPos))
          .foreach(coords => gGates.drawLine(mapHcoord(coords._1(0)), mapVcoord(coords._1(1)), mapHcoord(coords._2(0)), mapVcoord(coords._2(1))))
      }

    /** combine images into one */
    def timeReadable(t: Int): String = {
      val hours: Int = floor(t/3600000.0).toInt
      val minutes: Int = floor((t - hours*3600000)/60000.0).toInt
      val seconds: Double = t - hours*3600000 - minutes*60000
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    val gCombine: Graphics2D = combine.createGraphics()
    gCombine.drawImage(cleanCanvas, 0, 0, null)
    if (densityMeasurements.nonEmpty) { gCombine.drawImage(box, 0, 0, null) }
    gCombine.drawImage(points, 0, 0, null)
    gCombine.drawImage(gates, 0, 0, null)
    gCombine.setColor(Color.BLACK)
    gCombine.drawString(timeReadable(times2Show(i)), 200, 50)
    if (densityMeasurements.nonEmpty) { gCombine.drawString(densityMeasurements.head._2.toString, 200, 80) }

    // Removes the head of the history containers when the time of the current images reaches the time of the head of
    // the history containers.
    if (densityMeasurements.nonEmpty && densityMeasurements.tail.head._1 == times2Show(i)) {
      densityMeasurements = densityMeasurements.tail
    }

    if (gateHistory.nonEmpty && gateHistory.tail.head._1 == times2Show(i)) {
      gateHistory = gateHistory.tail
    }

    // pushes current image to encoder to make video
    enc.encodeImage(combine)
  }

  // closes encoder
  enc.finish()

}

class KPIEvolutionOverTime(outputFile: String,
                           size: (Int, Int),
                           fps: Int,
                           var data: collection.mutable.ArrayBuffer[(Time, Double)],
                           times2Show: Range) extends Tools4Videos {

  val borderSpacing: Int = 40
  val canvas: BufferedImage = new BufferedImage(size._1, size._2, BufferedImage.TYPE_4BYTE_ABGR)
  val gCanvas: Graphics2D = canvas.createGraphics()
  gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())

  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapHcoordAffine(times2Show.end, times2Show.head, canvas.getWidth-2*borderSpacing)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapVcoordLinear(1.5, canvas.getHeight-2*borderSpacing)

  /** Swaps the vertical coordinate, making the origin at the bottom left
    *
    * @return function taking a pixel and returning the swapped pixel
    */
  def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)


  def drawRotate(g2d: Graphics2D , x: Double, y: Double, angle: Int, text: String): Unit =
  {
    g2d.translate(x,y)
    g2d.rotate(Math.toRadians(angle))
    g2d.drawString(text,0,0)
    g2d.rotate(-Math.toRadians(angle))
    g2d.translate(-x,-y)
  }

  gCanvas.setColor(Color.BLACK)

  // axis
  gCanvas.drawLine(borderSpacing, verticalTransformation(borderSpacing), canvas.getWidth-borderSpacing+20, verticalTransformation(borderSpacing))
  gCanvas.drawLine(borderSpacing, verticalTransformation(borderSpacing), borderSpacing, verticalTransformation(canvas.getHeight-borderSpacing+20))

  // arrow heads
  gCanvas.drawLine(canvas.getWidth-borderSpacing+20, verticalTransformation(borderSpacing), canvas.getWidth-60+30, verticalTransformation(45))
  gCanvas.drawLine(canvas.getWidth-borderSpacing+20, verticalTransformation(borderSpacing), canvas.getWidth-60+30, verticalTransformation(35))
  gCanvas.drawLine(borderSpacing, verticalTransformation(canvas.getHeight-borderSpacing+20), 45, verticalTransformation(canvas.getHeight-60+30))
  gCanvas.drawLine(borderSpacing, verticalTransformation(canvas.getHeight-borderSpacing+20), 35, verticalTransformation(canvas.getHeight-60+30))

  // axis labels
  gCanvas.drawString("time", floor(canvas.getWidth*0.5).toInt, verticalTransformation(10))
  drawRotate(gCanvas, 12, verticalTransformation(floor(canvas.getHeight*0.5).toInt), 270, "density")//, 20, verticalTransformation(200))

  // xticks
  val nXTicks: Int = 5
  val nYTicks: Int = 5
  val xTicks: Range = times2Show.head to times2Show.end by (times2Show.end-times2Show.head)/nXTicks
  val yTicks: NumericRange[Double] = 0.0 to 1.5 by (1.5-0.0)/nYTicks
  val posXTicks: IndexedSeq[(Int, Int)] = xTicks map(mapHcoord(_))  zip xTicks
  val posYTicks: IndexedSeq[(Int, Double)] = yTicks map(mapVcoord(_)) zip yTicks

  posYTicks.foreach(tick => {
    gCanvas.drawLine(40, verticalTransformation(borderSpacing+tick._1), 45, verticalTransformation(borderSpacing+tick._1))
    gCanvas.drawString(f"${tick._2}%1.1f", 15, verticalTransformation(borderSpacing+tick._1-floor(0.33*gCanvas.getFontMetrics.getHeight).toInt))
  })

  posXTicks.foreach(tick => {
    gCanvas.drawLine(borderSpacing+tick._1, verticalTransformation(35), borderSpacing+tick._1, verticalTransformation(45))
    gCanvas.drawString(timeReadable(tick._2), borderSpacing+tick._1-floor(0.5*gCanvas.getFontMetrics.stringWidth(timeReadable(tick._2))).toInt, verticalTransformation(20))
  })

  gCanvas.setColor(Color.RED)

  val plot: Path2D = new Path2D.Double()
  plot.moveTo(borderSpacing+mapHcoord(times2Show.head), verticalTransformation(mapVcoord(0))-borderSpacing)

  val enc: AWTSequenceEncoder = AWTSequenceEncoder.createSequenceEncoder(new File(outputFile), fps)

  for (i <- times2Show.indices) {

    //if (100*times2Show(i)/(times2Show.end-times2Show.head).toInt % 5 == 0) {println(100*times2Show(i)/(times2Show.end-times2Show.head))}

    plot.lineTo(borderSpacing+mapHcoord(times2Show(i)), verticalTransformation(mapVcoord(data.head._2))-borderSpacing)
    gCanvas.draw(plot)

    if (data.tail.head._1 == times2Show(i)) {
      data = data.tail
    }
    enc.encodeImage(canvas)
  }
  enc.finish()
}