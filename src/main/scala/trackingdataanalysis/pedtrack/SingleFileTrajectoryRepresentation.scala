package trackingdataanalysis.pedtrack

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage

import hubmodel.Position
import trackingdataanalysis.TrackingDataFormat
import hubmodel.io.output.video.MovingPedestriansWithDensityWithWallVideo
import hubmodel.ped.History.CoordinateGroup
import javax.imageio.ImageIO
import trackingdataanalysis.visualization.{DrawingComponents, PlotOptions}
import hubmodel.io.output.createBackgroundFromImage

/** Reads a single file of trajectory data and visualizes it.
  *
  */
class SingleFileTrajectoryRepresentation(file: String) extends TrajectoryProcessing {

  // try and guess the data format from the file
  val dataType: TrackingDataFormat = guessDataFormat(file)

  // read the data file and build the population
  val population: Map[Int, hubmodel.ped.Pedestrian] = buildPedestrianTrajectories(dataType)

  population.head._2.entryTime
  val (start, end) = population.values.foldRight((population.head._2.entryTime.value,population.head._2.exitTime.value))((ped, acc) => (acc._1.min(ped.entryTime.value), acc._2.max(ped.exitTime.value)))

  /*new MovingPedestriansWithDensityWithWallVideo(
    "test.mp4",
    Vector(),
    5,
    population.values.toVector,
    Vector(),
    Map(),
    collection.mutable.ArrayBuffer(),
    scala.collection.mutable.ArrayBuffer(),
    (start to end by BigDecimal(1.0)).map(t => hubmodel.tools.Time(t.toDouble)).toVector,
    Vector()
  )*/

  def functionGroup2(p: Position): Position = {
    p + 50
  }

population.values.map(ped => {
  ped.getHistoryPosition.map(h => {
    h._2 match {
      case g: CoordinateGroup => {
        if (g.group == 2) {g.pos}
        else if (g.group == 3) {}
        else if (g.group == 4) {}
        else {}
      }
    }
  })
})

  new TrajectoriesImage(
    "test.png",
    population,
    "traj",
    "",
    "",
    "title",
    bckgImage = Some("/home/nicholas/bern-tracking-data/underpass.png")
  )
}

class TrajectoriesImage(outputFile: String,
                        pop: Map[Int, hubmodel.ped.Pedestrian],
                        label: String,
                        xLabel: String,
                        yLabel: String,
                        title: String,
                        opts: PlotOptions = PlotOptions(),
                        bckgImage: Option[String] = None) extends DrawingComponents(opts.border2HorizontalAxis, opts.border2VerticalAxis, (opts.width, opts.height)) {


  val boundsData: (Double, Double, Double, Double) = pop.values
      .foldRight((pop.head._2.getHistoryPosition.head._2.pos.X,pop.head._2.getHistoryPosition.head._2.pos.Y,pop.head._2.getHistoryPosition.head._2.pos.X,pop.head._2.getHistoryPosition.head._2.pos.Y))((ped, accOuter) => {
        val pedBounds = ped.getHistoryPosition
          .foldRight((ped.getHistoryPosition.head._2.pos.X,ped.getHistoryPosition.head._2.pos.Y,ped.getHistoryPosition.head._2.pos.X,ped.getHistoryPosition.head._2.pos.Y))((xyt, acc) => (acc._1.min(xyt._2.pos.X), acc._2.min(xyt._2.pos.Y), acc._3.max(xyt._2.pos.X), acc._4.max(xyt._2.pos.Y)) )
        (accOuter._1.min(pedBounds._1), accOuter._2.min(pedBounds._2), accOuter._3.max(pedBounds._3), accOuter._4.max(pedBounds._4))
      })

  val bounds = (0,0,100,50)

  // builds the background based on the size passed as argument
  val canvas: BufferedImage = if (bckgImage.isDefined) {
    createBackgroundFromImage(bckgImage, (bounds._3-bounds._1,bounds._4-bounds._2))
  } else {
    new BufferedImage(pixelCanvasSize._1 + 2 * (0.075 * pixelCanvasSize._1).toInt, pixelCanvasSize._2, BufferedImage.TYPE_4BYTE_ABGR)
  }

  val gCanvas: Graphics2D = canvas.createGraphics()
  /*gCanvas.setColor(Color.WHITE)
  gCanvas.fillRect(0, 0, canvas.getWidth(), canvas.getHeight())*/

  // completes abstract classes by implementing the mapping functions
  def verticalTransformation: Int => Int = verticalMirrorTransformation(canvas.getHeight)

  def mapHCoordBD(v: BigDecimal): Int = 0

  def mapVCoordBD(v: BigDecimal): Int = 0


  def mapHcoordDrawingZoneBD(v: BigDecimal): Int = 0

  def mapVcoordDrawingZoneBD(v: BigDecimal): Int = 0


  def mapHCoord(x: Double): Int = mapHcoordLinearBD(bounds._1, bounds._3, canvas.getWidth - border2HAxis - border2VAxis)(x)

  def mapVCoord(x: Double): Int = mapVcoordLinearBD(bounds._2, bounds._4, canvas.getHeight - 2 * border2HAxis)(x)

  def mapHcoordDrawingZone(v: Double): Int = mapHcoordAffine(bounds._1, bounds._3, border2VAxis, canvas.getWidth - border2HAxis)(v)

  def mapVcoordDrawingZone(v: Double): Int = mapHcoordAffine(bounds._2, bounds._4, border2HAxis, canvas.getHeight - border2VAxis)(v)


  // coloring scheme
  def coloring(g: Int): Color = {
    if (g == 2) {Color.BLUE}
    else if (g == 3) {Color.RED}
    else if (g == 4) {Color.GREEN}
    else {Color.BLACK}
  }

  drawTrajectories(gCanvas, pop, coloring)


  // Writes image to file
  if (outputFile.length > 0) {
    ImageIO.write(canvas, outputFile.split("\\.").last, new java.io.File(outputFile))
  }
}
