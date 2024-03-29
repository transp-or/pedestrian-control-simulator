package hubmodel.io.output.video

import java.awt.geom._
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D}
import java.io.File

import hubmodel.DES.PedestrianDES
import hubmodel._
import hubmodel.control.flowgate.BinaryGate
import hubmodel.control.flowsep.FlowSeparator
import hubmodel.io.output.{createWhiteBackground, getBounds, mapCoordAffine, verticalMirrorTransformation}
import hubmodel.ped.History.{CoordinateTracking, HistoryContainer, PositionIsolation}
import hubmodel.ped.PedestrianTrajectory
import hubmodel.supply.continuous.Wall
import org.jcodec.api.awt.AWTSequenceEncoder
import tools.Time
import tools.cells.DensityMeasuredArea

/** Creates a video showing the movement of individual pedestrians with the critical zones in which the density is
  * monitored accompanied with the gates. Consider replacing the long list of arguments by the [[PedestrianDES]] object which
  * contains all the information needed for creating the video.
  *
  * The images are based on the times in the times2Show parameter. These times are used to build the timeEllipses
  * variable. One image is created for each value in the times2Show range.
  *
  * @param outputFile          name of the video
  * @param walls               collection of walls to plot
  * @param fps                 The number of frames per second
  * @param pop                 The set of pedestrians to draw
  * @param criticalAreaInput   The critical area in which the density is monitored
  * @param gateCollection      the collection of gates which are used to limit inflow into specific areas
  * @param gateHistory         the status over time of the gates (open or closed)
  * @param densityMeasurements Density values inside the critical zone.
  * @param times2Show          The set of times to show.
  */
class MovingPedestriansWithDensityWithWallVideo(outputFile: String,
                                                walls: Iterable[Wall],
                                                fps: Int,
                                                pop: Vector[PedestrianTrajectory],
                                                criticalAreaInput: Iterable[DensityMeasuredArea],
                                                gateCollection: Map[String, BinaryGate],
                                                var gateHistory: scala.collection.mutable.ArrayBuffer[(Int, List[(String, Boolean)])],
                                                var densityMeasurements: collection.mutable.ArrayBuffer[(Int, Double)],
                                                times2Show: IndexedSeq[Time],
                                                flowSeparators: Iterable[FlowSeparator[_, _]]) extends Tools4Videos {

  // asserts that more than one time is listed
  assert(times2Show.size > 0.0)
  println(" * writing " + times2Show.size + " frames at " + fps + " frames per second.")

  // boundaries defined by the walls
  val bounds: (Double, Double, Double, Double) = if (walls.nonEmpty) {
    getBounds(walls)
  } else {
    pop
      .foldRight((pop.head.getHistoryPosition.head._2.pos.X,pop.head.getHistoryPosition.head._2.pos.Y,pop.head.getHistoryPosition.head._2.pos.X,pop.head.getHistoryPosition.head._2.pos.Y))((ped, accOuter) => ped.getHistoryPosition
        .foldRight((ped.getHistoryPosition.head._2.pos.X,ped.getHistoryPosition.head._2.pos.Y,ped.getHistoryPosition.head._2.pos.X,ped.getHistoryPosition.head._2.pos.Y))((xyt, acc) => (acc._1.min(xyt._2.pos.X), acc._2.min(xyt._2.pos.Y), acc._3.max(xyt._2.pos.X), acc._4.max(xyt._2.pos.Y)) ))
  }
  val widthMeters: Double = bounds._3 - bounds._1
  val heightMeters: Double = bounds._4 - bounds._2

  println(" * real world size: width=" + widthMeters + "m, height=" + heightMeters + "m")

  // background canvas containing the walls to draw on
  val cleanCanvas: BufferedImage = createWhiteBackground((widthMeters, heightMeters))

  val canvasWidth: Int = cleanCanvas.getWidth
  val canvasHeight: Int = cleanCanvas.getHeight


  println(" * canvas size: width=" + canvasWidth + "px, height=" + canvasHeight + "px")

  val verticalTransformation: Int => Int = verticalMirrorTransformation(cleanCanvas.getHeight)

  val wallsImage: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
  val gWallsImage: Graphics2D = wallsImage.createGraphics()
  gWallsImage.setColor(Color.BLACK)
  walls.foreach(w => gWallsImage.drawLine(
    mapHcoord(w.startPoint.X),
    verticalTransformation(mapVcoord(w.startPoint.Y)),
    mapHcoord(w.endPoint.X),
    verticalTransformation(mapVcoord(w.endPoint.Y))
  )
  )


  // computes the siize of a dot (person in pixels)
  val dotSize: Double = (0.35 / widthMeters) * canvasWidth.toDouble

  // formatting of the data: aggregation by times of the positions.
  val population2TimePositionList: List[(Time, List[HistoryContainer])] = mergeListsByTime(pop.flatMap(_.getHistoryPosition).toList).sortBy(_._1)
  // List of times with corresponding ellipses to draw. For each time, the list of ellipses coreespond to the pedestrians.

  val timeEllipses: Vector[(Time, List[(Ellipse2D, Option[Int])])] = population2TimePositionList
    .filter(pair => times2Show.exists(t => (t - pair._1).abs.value <= math.pow(10, -5)))
    .map(p => (p._1, p._2.map({case p: CoordinateTracking => {(createDot(p.pos), None)} case pi: PositionIsolation => {(createDot(pi.pos), Some(pi.isolationType))}})))
    .toVector

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
    timeEllipses.indexWhere(te => (times2Show(i) - te._1).abs.value < math.pow(10, -5)) match {
      case a if a >= 0 => timeEllipses(a)._2.foreach(p => {
        p._2 match {
          case Some(IN_COLLISION)=> {gPoints.setColor(Color.RED)}
          case Some(IN_RANGE) => {gPoints.setColor(Color.YELLOW)}
          case Some(ISOLATED) => {gPoints.setColor(Color.GREEN)}
          case None => {gPoints.setColor(Color.BLUE)}
        }
        gPoints.fill(p._1)
      })

      case _ => {}
    }
    //timeEllipses = timeEllipses.tail
    //}

    /** Draws colored boxes showing densities inside areas */

    val box: BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (criticalAreaInput.nonEmpty) {
      criticalAreaInput.foreach(criticalArea => {
        if (criticalArea.densityHistory.head._1 == times2Show(i)) {
          val gBox: Graphics2D = box.createGraphics()
          if (criticalArea.densityHistory.head._2 >= 2.17) gBox.setColor(new Color(153, 0, 0, 25))
          else if (criticalArea.densityHistory.head._2 > 1.08) gBox.setColor(new Color(255, 0, 0, 25))
          else if (criticalArea.densityHistory.head._2 > 0.72) gBox.setColor(new Color(255, 128, 0, 25))
          else if (criticalArea.densityHistory.head._2 > 0.43) gBox.setColor(new Color(255, 255, 0, 25))
          else if (criticalArea.densityHistory.head._2 > 0.31) gBox.setColor(new Color(0, 255, 0, 25))
          else if (criticalArea.densityHistory.head._2 <= 0.31) gBox.setColor(new Color(0, 0, 255, 25))
          val areaCoords: (Position, Position, Position, Position) = (criticalArea.corners(0), criticalArea.corners(1), criticalArea.corners(2), criticalArea.corners(3))
          gBox.fillRect(mapHcoord(areaCoords._1.X), verticalTransformation(mapVcoord(areaCoords._1.Y)), mapHcoord(areaCoords._3.X) - mapHcoord(areaCoords._4.X), verticalTransformation(mapVcoord(areaCoords._4.Y) - mapVcoord(areaCoords._1.Y)))
          gBox.setColor(Color.BLACK)
          gBox.drawRect(mapHcoord(areaCoords._1.X), verticalTransformation(mapVcoord(areaCoords._1.Y)), mapHcoord(areaCoords._3.X) - mapHcoord(areaCoords._4.X), verticalTransformation(mapVcoord(areaCoords._4.Y) - mapVcoord(areaCoords._1.Y)))
          criticalArea.densityHistory.drop(1)
        }
      })
    }


    /** Draws flow separators */
    val flowSepImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_4BYTE_ABGR)
    if (flowSeparators.nonEmpty) {
      val gFlowSepImage = flowSepImage.createGraphics()
      flowSeparators.foreach(fg => {
        fg.getPositionHistory.reverse.find(t => times2Show(i).value > t._1.value) match {
          case Some(s) => {
            gFlowSepImage.setStroke(new BasicStroke(5))
            gFlowSepImage.setColor(Color.BLACK)
            gFlowSepImage.drawLine(
              mapHcoord(s._2.X),
              verticalTransformation(mapVcoord(s._2.Y)),
              mapHcoord(s._3.X),
              verticalTransformation(mapVcoord(s._3.Y))
            )
          }
          case _ => {}
        }
      })
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
        .foreach(coords => gGates.drawLine(mapHcoord(coords._1.X), verticalTransformation(mapVcoord(coords._1.Y)), mapHcoord(coords._2.X), verticalTransformation(mapVcoord(coords._2.Y))))
    }

    val gCombine: Graphics2D = combine.createGraphics()
    gCombine.drawImage(cleanCanvas, 0, 0, null)
    gCombine.drawImage(wallsImage, 0, 0, null)
    if (densityMeasurements.nonEmpty) {
      gCombine.drawImage(box, 0, 0, null)
    }

    gCombine.drawImage(points, 0, 0, null)
    gCombine.drawImage(gates, 0, 0, null)
    gCombine.drawImage(flowSepImage, 0, 0, null)
    gCombine.setColor(Color.BLACK)
    gCombine.drawString(times2Show(i).asReadable, 200, 50)
    if (densityMeasurements.nonEmpty) {
      gCombine.drawString(densityMeasurements.head._2.toString, 200, 80)
    }

    // Removes the head of the history containers when the time of the current images reaches the time of the head of
    // the history containers.
    if (densityMeasurements.size > 1 && densityMeasurements.tail.head._1 == times2Show(i).value) {
      densityMeasurements = densityMeasurements.tail
    }

    if (gateHistory.nonEmpty && gateHistory.tail.head._1 == times2Show(i).value) {
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


  /** Horizontal mapping of coordinates
    *
    * @return function taking a horizontal position and returning the position in pixels
    */
  def mapHcoord: Double => Int = mapCoordAffine(bounds._1, bounds._3, canvasWidth)

  /** Vertical mapping of coordinates
    *
    * @return function taking a vertical position and returning the position in pixels
    */
  def mapVcoord: Double => Int = mapCoordAffine(bounds._2, bounds._4, canvasHeight)

  /** Function which returns an [[Ellipse2D]] to draw at a specific location
    *
    * @return Function taing a [[Position]] and returning an [[Ellipse2D]] representing a pedestrian at a specific location
    */
  def createDot: Position => Ellipse2D = createDot((mapHcoord, (d: Double) => verticalTransformation(mapVcoord(d))), dotSize)

  /** combine images into one */
  /*def timeReadable(t: Double): String = {
    val hours: Int = floor(t / 3600.0).toInt
    val minutes: Int = floor((t - hours * 3600) / 60.0).toInt
    val seconds: Double = t - hours * 3600 - minutes * 60
    hours.toString + ":" + minutes.toString + ":" + seconds.toString
  }*/
}
