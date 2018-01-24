import java.io.File

import breeze.numerics.pow
import hubmodel.input.demand.{SplitFractions, TimeTable, Train, TrainTimeTable, TrainTypeSplitFractions}
import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic.Tolerance._
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import java.time.LocalTime

object disaggregateDemand extends App {


  case class Node(area: String, obj: String = "NA", modifier: String = "NA") {
    override def toString: String = {
      if (modifier != "NA" && obj != "NA") area + "-" + obj + "-" + modifier
      else if (modifier == "NA" && obj != "NA") area + "-" + obj
      else if (modifier == "NA" && obj == "NA") area
      else area + "-" + obj + "-" + modifier
    }
  }

  case class ODValue(O: Node, D: Node, flow: Double)


  /** Container for the command line arguments.
    *
    * @param schedule file containing the schedule
    */
  case class Config(schedule: File = new File("."), outputFile: File = new File("."), lower: String = "", upper: String = "", verbose: Boolean = false)

  val parser = new scopt.OptionParser[Config]("disaggregateDemand") {
    head("Disaggregation of pedestrian OD demand into station exit/entrances.")

    opt[File]('s', "schedule").required().valueName("<file>")
      .action((x, c) => c.copy(schedule = x))
      .text("required, JSON specification of the train schedule")

    opt[File]('o', "output").required().valueName("<file>")
      .action((x, c) => c.copy(outputFile = x))
      .text("required, file to save the OD matrix")

    opt[String]('u', "upper").optional().valueName("<string>")
      .action((x, c) => c.copy(upper = x))
      .text("optional, upper bound on the train time table")

    opt[String]('l', "lower").optional().valueName("<string>")
      .action((x, c) => c.copy(lower = x))
      .text("optional, lower bound on the train time table")

    opt[Unit]('v', "verbose").action((_, c) =>
      c.copy(verbose = true)).text("prints a summary of the data and OD matrix")

    help("help").text("prints this usage text\n\n" +
      "  This function is calibrated for the Lausanne 2025 station only.\n" +
      "  The hypotheses which are used for disaggregating the flows to\n" +
      "  specific nodes are the following:\n" +
      "  - alighting flows:\n" +
      "    - puw, puc -> M3: can't reach northbound platform, hence goes outside\n" +
      "    - puw, puc -> bus: uses the ramp at north of station and stairs inside PU\n" +
      "    - puw, puc -> walk: pedestrian leaving on foot use the three exit ways at the north area\n" +
      "    - passengers exiting on foot via the south of the station use all the exits from all pu\n" +
      "    - pue -> M3: southbound passengers cannot reach the platform from inside, hence they leave the sation\n" +
      "    - pue -> M2: they stay inside the station to reach the M2\n" +
      "    - transfer passengers stay in the same pu\n" +
      "  - boarding flows to trains from:\n" +
      "    - south-weast -> puw\n" +
      "    - south-east-1 -> pue\n" +
      "    - south-east-2 -> pue + puc\n" +
      "    - ramp-north -> puc + puw\n" +
      "    - other north entrances -> closest pu\n" +
      "    - bus -> closest middle stairs (stairs inside pu)\n" +
      "    - M2-southbound-1 -> puw\n" +
      "    - M2-southbound-2 -> puw + puc\n" +
      "    - M2-northbound-1 -> puc\n" +
      "    - M2-northbound-2 -> puc\n" +
      "    - M2-northbound-3 -> pue\n" +
      "    - M3 -> cloest pu\n\n" +
      "  For more details, investigate the maps in the source code !")
  }

  val inputParameters: (File, File, String, String, Boolean) = parser.parse(args, Config()) match {
    case Some(c) => (c.schedule, c.outputFile, c.lower, c.upper, c.verbose)
    case None => throw new RuntimeException("Error while parsing CLI !")
  }

  val verbose: Boolean = inputParameters._5

  val timeLowerBound =
    if (inputParameters._3.isEmpty) LocalTime.parse("00:00:00")
    else LocalTime.parse(inputParameters._3)

  val timeUpperBound =
    if (inputParameters._4.isEmpty) LocalTime.parse("23:59:59")
    else LocalTime.parse(inputParameters._4)


  val splitFractions: Map[String, Map[String, Map[String, Double]]] = {
    val source: BufferedSource = scala.io.Source.fromFile(inputParameters._1)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    (input.validate[SplitFractions] match {
      case s: JsSuccess[SplitFractions] => s.get
      case e: JsError => throw new Error("Error while parsing split fractions of train types: " + JsError.toJson(e).toString())
    }).fractions.map(t => t.trainType -> t.influence.map(i => i.track -> i.access.map(a => a._1 -> a._2).toMap).toMap).toMap
  }

  splitFractions.foreach(trainType => trainType._2.foreach(plat => {
    assert(plat._2.values.foldLeft(0.0)((tot: Double, a) => tot + a) === 1.0 +- 1.0 / pow(10, 8), trainType._1 + ", " + plat._1 + " => sum != 1.0")
  }))

  val timeTable: Map[String, Train] = {
    val source: BufferedSource = scala.io.Source.fromFile(inputParameters._1)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[TrainTimeTable] match {
      case s: JsSuccess[TrainTimeTable] => s.get.timeTable.filter(train => {
        (train._2.arr match {
          case Some(t) => t.isAfter(timeLowerBound)
          case None => train._2.dep.get.isAfter(timeLowerBound)
        }) &&
          (train._2.dep match {
            case Some(t) => t.isBefore(timeUpperBound)
            case None => train._2.arr.get.isBefore(timeUpperBound)
          })
      })
      case e: JsError => throw new Error("Error while parsing train timestable: " + JsError.toJson(e).toString())
    }
  }

  val ODFractions: Map[String, Map[String, Double]] = Map(
    "train" -> Map("train" -> 0.16416, "south" -> 0.11705, "M2" -> 0.20850, "M3" -> 0.20850, "walk" -> 0.19402, "bus" -> 0.10777),
    "south" -> Map("train" -> 0.13986),
    "M2" -> Map("train" -> 0.24942),
    "M3" -> Map("train" -> 0.24942),
    "walk" -> Map("train" -> 0.23232),
    "bus" -> Map("train" -> 0.12898))

  assert(ODFractions("train").foldLeft(0.0)((tot: Double, a) => tot + a._2) === 1.0 +- 1.0 / pow(10, 8), "OD fractions do not sum up to 1.0 for arriving passengers")
  assert((ODFractions("south")("train") + ODFractions("M2")("train") + ODFractions("M3")("train") + ODFractions("walk")("train") + ODFractions("bus")("train")) === 1.0 +- 1.0 / pow(10, 8), "OD fractions do not sum up to 1.0 for boarding passengers")

  val platformModifier: Map[Int, String] = Map(1 -> "pl1", 2 -> "pl23", 3 -> "pl23", 4 -> "pl45", 5 -> "pl45", 6 -> "pl67", 7 -> "pl67", 8 -> "pl8")

  val area2NodesAlighting: Map[(String, String), Iterable[Node]] = Map(
    ("puw", "M3") -> List(Node("north", "stairs-north"), Node("north", "stairs-middle-west"), Node("north", "M3", "southbound")), // from puw, pedestrians go directly to the southbound M3 stop or must go up and out to reach the northbound platform of the M3.
    ("puw", "M2") -> List(Node("north", "M2-southbound", "1"), Node("north", "M2-southbound", "2"), Node("north", "M2-northbound", "1"), Node("north", "M2-northbound", "2")), // form puw, pedestrians can go to any of the stairs or ramps connecting the M2.
    ("puw", "walk") -> List(Node("north", "stairs-west"), Node("north", "stairs-north"), Node("north", "ramp-north")), // from puw, pedestrians who leave the station walking will take the specified exits
    ("puw", "bus") -> List(Node("north", "stairs-middle-west"), Node("north", "ramp-north")), // from puw, passengers taking the bus will use these exits
    ("puw", "train") -> List(1, 2, 3, 4, 5, 6, 7, 8).flatMap(track => List(Node("puw", platformModifier(track) + "-" + "ramp-west"), Node("puw", platformModifier(track) + "-" + "stairs-east"))),
    ("puc", "M3") -> List(Node("north", "stairs-north"), Node("north", "stairs-middle-center"), Node("north", "M3", "southbound")), // from puc, pedestrians go directly to the southbound M3 stop or must go up and out to reach the northbound platform of the M3.
    ("puc", "M2") -> List(Node("north", "M2-southbound", "2"), Node("north", "M2-northbound", "1"), Node("north", "M2-northbound", "2")), // form puc, pedestrians can go to any of the stairs or ramps connecting the M2.
    ("puc", "walk") -> List(Node("north", "stairs-west"), Node("north", "stairs-north"), Node("north", "ramp-north")), // from puc, pedestrians who leave the station walking will take the specified exits
    ("puc", "bus") -> List(Node("north", "stairs-middle-center"), Node("north", "ramp-north")), // from puc, passengers taking the bus will use these exits
    ("puc", "train") -> List(1, 2, 3, 4, 5, 6, 7, 8).flatMap(track => List(Node("puc", platformModifier(track) + "-" + "stairs-west"), Node("puc", platformModifier(track) + "-" + "stairs-east"))),
    ("pue", "M3") -> List(Node("north", "M3-northbound"), Node("north", "stairs-middle-east")), // from pue, passengers for M3 either go there directly, or go outside for the southbound platform.
    ("pue", "M2") -> List(Node("north", "M2-northbound", "3"), Node("north", "M2-southbound", "2")), // from pue, passengers for M2 stay inside PUs.
    ("pue", "walk") -> List(Node("north", "stairs-middle-east"), Node("north", "stairs-east", "1"), Node("north", "stairs-east", "2")), // passengers use only these exits form the station
    ("pue", "bus") -> List(Node("north", "stairs-middle-east")),
    ("pue", "train") -> (Node("pue", "pl8-stairs-west") :: List(1, 2, 3, 4, 5, 6, 7).flatMap(track => List(Node("pue", platformModifier(track) + "-" + "stairs-west"), Node("pue", platformModifier(track) + "-" + "stairs-east")))),
    ("puw", "south") -> List(Node("south", "stairs-west"), Node("south", "stairs-east", "1"), Node("south", "stairs-east", "2")),
    ("puc", "south") -> List(Node("south", "stairs-west"), Node("south", "stairs-east", "1"), Node("south", "stairs-east", "2")),
    ("pue", "south") -> List(Node("south", "stairs-west"), Node("south", "stairs-east", "1"), Node("south", "stairs-east", "2"))
  )

  val area2NodesBoarding: Map[String, Map[Node, Vector[String]]] = Map(
    "south" -> Map(
      Node("south", "stairs-west") -> Vector("puw"),
      Node("south", "stairs-east", "1") -> Vector("pue"),
      Node("south", "stairs-east", "2") -> Vector("pue", "puc")
    ),
    "walk" -> Map(
      Node("north", "stairs-west") -> Vector("puw"),
      Node("north", "stairs-north") -> Vector("puc"),
      Node("north", "ramp-north") -> Vector("puw", "puc"),
      Node("north", "stairs-east", "1") -> Vector("pue"),
      Node("north", "stairs-east", "2") -> Vector("pue")
    ),
    "bus" -> Map(
      Node("north", "stairs-middle-west") -> Vector("puw"),
      Node("north", "stairs-middle-center") -> Vector("puc"),
      Node("north", "stairs-middle-east") -> Vector("pue")
    ),
    "M2" -> Map(
      Node("north", "M2-southbound", "1") -> Vector("puw"),
      Node("north", "M2-southbound", "2") -> Vector("puw", "puc"),
      Node("north", "M2-northbound", "1") -> Vector("puc"),
      Node("north", "M2-northbound", "2") -> Vector("puc"),
      Node("north", "M2-northbound", "3") -> Vector("pue")

    ),
    "M3" -> Map(
      Node("north", "M3-southbound") -> Vector("puc"),
      Node("north", "M3-northbound") -> Vector("pue")
    )
  )

  // set of accessways onto platforms as a function of the PU and the track number. Used for boarding flows
  def areas2accessways(area: String, track: Int): Vector[String] = {
    if (area == "puw" && track == 8) Vector("ramp-west", "stairs-west", "stairs-east")
    else if (area == "puw" && track != 8) Vector("ramp-west", "stairs-east")
    else if (area == "puc") Vector("stairs-west", "stairs-east")
    else if (area == "pue" && track == 8) Vector("stairs-west")
    else if (area == "pue" && track != 8) Vector("stairs-west", "stairs-east")
    else throw new RuntimeException("error in mapping" + ", " + area + ", " + track)
  }

  if (verbose) {
    println("*****************************************************\nThe following trains where used to generate the OD matrix:")
    println(timeTable.values.mkString("\n"))
  }

  val ODMatrix: Map[(Node, Node), Double] =
    (
      timeTable.values.flatMap(train => {
        splitFractions(train.trainType)(train.track.toString).map(f => Node(f._1.split("-").head, platformModifier(train.track) + "-" + f._1.split("-")(1) + "-" + f._1.split("-")(2)) -> f._2 * 0.7 * train.capacity).flatMap(OFlow => {
          ODFractions("train").map(D => ODValue(OFlow._1, Node(D._1), OFlow._2 * D._2))
        })
      }
      ).flatMap(od => {
        area2NodesAlighting((od.O.area, od.D.area)).map(newD => ODValue(od.O, newD, od.flow / area2NodesAlighting((od.O.area, od.D.area)).size))
      }
      )
        ++
        timeTable.values.flatMap(train => {
          (ODFractions - "train").flatMap(Oarea => area2NodesBoarding(Oarea._1).flatMap(O => O._2.flatMap(Darea => {
            areas2accessways(Darea, train.track).map(access => ODValue(O._1, Node(Darea, platformModifier(train.track) + "-" + access), 0.4 * train.capacity * Oarea._2.head._2 / (O._2.size * areas2accessways(Darea, train.track).size * area2NodesBoarding(Oarea._1).size)))
          })))
        }
        )
      ).groupBy(OD => (OD.O, OD.D)).map(od => od._1 -> od._2.foldLeft(0.0)((a: Double, b: ODValue) => a + b.flow))

  assert(
    (timeTable.values.foldLeft(0.0)((tot: Double, t) => tot + t.capacity * 0.7) + timeTable.values.foldLeft(0.0)((tot: Double, t) => tot + t.capacity * 0.4))
      ===
      ODMatrix.foldLeft(0.0)((a: Double, b) => a + b._2) +- 1.0 / pow(10, 8),
    "Pedestrian have disappeard during the disaggregation process ! origin=" + timeTable.values.foldLeft(0.0)((tot: Double, t) => tot + t.capacity * 0.7) + timeTable.values.foldLeft(0.0)((tot: Double, t) => tot + t.capacity * 0.4) + ", final=" + ODMatrix.foldLeft(0.0)((a: Double, b) => a + b._2)
  )

  val numberODs: Int = ODMatrix.keys.flatMap(k => List(k._1.toString, k._2.toString)).toVector.distinct.size
  val ODNames: Vector[String] = ODMatrix.keys.flatMap(k => List(k._1.toString, k._2.toString)).toVector.distinct.sortBy(_.toString)
  val name2index: Map[String, Int] = ODNames.zipWithIndex.toMap
  val ODMatrixAsMatrix: Array[Array[Double]] = Array.ofDim[Double](numberODs, numberODs)

  if (verbose) {
    println("*****************************************************\nThere are " + numberODs + " different nodes:")
    println(ODNames.mkString("\n"))
  }

  ODMatrix.foreach(od => ODMatrixAsMatrix(name2index(od._1._1.toString))(name2index(od._1._2.toString)) = od._2)

  ODMatrixAsMatrix.map(a => a.toVector).toVector.writeToCSV(
    inputParameters._2.toString,
    Option(ODMatrix.keys.flatMap(k => List(k._1.toString, k._2.toString)).toVector.distinct.sorted),
    Option("OD" +: ODMatrix.keys.flatMap(k => List(k._1.toString, k._2.toString)).toVector.distinct.sorted)
  )
}


