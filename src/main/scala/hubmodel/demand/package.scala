import java.nio.file.{DirectoryStream, Files, Path, Paths}
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, LocalTime}

import com.typesafe.config.Config
import hubmodel.demand.transit.Vehicle
import hubmodel.io.input.JSONReaders.PublicTransportScheduleReader
import hubmodel.io.input.JSONReaders.TRANSFORM.{PedestrianCollectionReaderTF, PublicTransportScheduleReaderTF}
import hubmodel.supply.{NodeID_New, StopID_New, TrainID_New}
import hubmodel.tools.Time
import hubmodel.tools.TimeNumeric.mkOrderingOps
import hubmodel.tools.cells.Rectangle
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.io.BufferedSource
import scala.jdk.CollectionConverters._


/**
  * Created by nicholas on 3/8/17.
  */
package hubmodel {

  import hubmodel.io.input.JSONReaders.TRANSFORM.Vehicle_JSON_TF
  import hubmodel.tools.exceptions.IllegalSimulationInput


  package object demand {

    val string2LocalDateTime: String => LocalDateTime = str => LocalDateTime.parse(str, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))


    implicit val localDateTimeFormat = new Format[LocalDateTime] {
      override def reads(json: JsValue): JsResult[LocalDateTime] =
        json.validate[String].map(str => LocalDateTime.parse(str, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")))

      override def writes(o: LocalDateTime): JsValue = Json.toJson(o.toString)
    }


    val string2LocalTime: String => LocalTime = str => LocalTime.parse(str, DateTimeFormatter.ofPattern("HH:mm:ss"))


    implicit val localTimeFormat = new Format[LocalTime] {
      override def reads(json: JsValue): JsResult[LocalTime] =
        json.validate[String].map(str => LocalTime.parse(str, DateTimeFormatter.ofPattern("HH:mm:ss")))

      override def writes(o: LocalTime): JsValue = Json.toJson(o.toString)
    }


    //type Flow = (Double, (Double, Double))
    //type NodalFlow = (NodeID, Flow)
    //type ODFlow = (NodeID, NodeID, Flow)


    //class train(val ID: Int, val capacity: Double, val connectionLocation: Vector[Int], val doorCapacity: Double) extends PTRun

    /* ----------------------------------------------------------------------------------
                                      TRAIN TIMETABLE
    -----------------------------------------------------------------------------------*/

    /*implicit val trainReads: Reads[transit.Train] = (
      (JsPath \ "id").read[String](minLength[String](1)) and
        (JsPath \ "type").read[String] and
        (JsPath \ "track").read[Int](min(0)) and
        (JsPath \ "arrival-time").readNullable[LocalTime] and
        (JsPath \ "departure-time").readNullable[LocalTime] and
        (JsPath \ "capacity").read[Int](min(0))
      ) (Train.apply _)

    implicit val trainTimeTableReads: Reads[TrainTimeTable] = (
      (JsPath \ "location").read[String](minLength[String](1)) and
        (JsPath \ "trains").read[Vector[transit.Train]]
      ) (TrainTimeTable.apply _)

    implicit val Track2NodesReads: Reads[Track2Nodes] = (
      (JsPath \ "track").read[Int] and
        (JsPath \ "nodes").read[Vector[NodeID]]
      ) (Track2Nodes.apply _)

    implicit val track2nodeMappingReads: Reads[Track2NodeMapping] = (
      (JsPath \ "location").read[String] and
        (JsPath \ "track2nodes").read[Vector[Track2Nodes]]
      ) (Track2NodeMapping.apply _)*/


    /* ----------------------------------------------------------------------------------
                                    PASSENGER OD DATA
    -----------------------------------------------------------------------------------*/

    implicit val PTFlowReads: Reads[PTFlow] = (
      (JsPath \ "origin").read[String](minLength[String](1)) and
        (JsPath \ "destination").read[String](minLength[String](1)) and
        (JsPath \ "flow").read[Double](min(0.0))
      ) (PTFlow.apply _)


    implicit val ExternalFlowReads: Reads[PedestrianFlow] = (
      (JsPath \ "origin").read[String] and
        (JsPath \ "destination").read[String] and
        (JsPath \ "start").read[LocalTime] and
        (JsPath \ "end").read[LocalTime] and
        (JsPath \ "flow").read[Double](min(0.0))
      ) (PedestrianFlow.apply _)

    implicit val SinusFunctionReads: Reads[SinusFunction] = (
      (JsPath \ "start").read[LocalTime] and
        (JsPath \ "end").read[LocalTime] and
        (JsPath \ "period_multiplier").read[Double] and
        (JsPath \ "period_shift").read[Double] and
        (JsPath \ "a").read[Double] and
        (JsPath \ "b").read[Double] and
        (JsPath \ "c").read[Double] and
        (JsPath \ "max_flow").read[Double]
      ) (SinusFunction.apply _)

    implicit val LinearFunctionReads: Reads[LinearFunction] = (
      (JsPath \ "start").read[LocalTime] and
        (JsPath \ "end").read[LocalTime] and
        (JsPath \ "rate_at_start").read[Double] and
        (JsPath \ "rate_at_end").read[Double] and
        (JsPath \ "slope").read[Double]
      ) (LinearFunction.apply _)

    implicit val ConstantFunctionReads: Reads[ConstantFunction] = (
      (JsPath \ "start").read[LocalTime] and
        (JsPath \ "end").read[LocalTime] and
        (JsPath \ "rate").read[Double](min(0.0))
      ) (ConstantFunction.apply _)

    implicit val PedestrianFlowFunctionReads: Reads[PedestrianFlowFunction] = (
      (JsPath \ "origin").read[String] and
        (JsPath \ "destination").read[String] and
        (JsPath \ "constant").read[Vector[ConstantFunction]] and
        (JsPath \ "linear").read[Vector[LinearFunction]] and
        (JsPath \ "sine").read[Vector[SinusFunction]]
      ) (PedestrianFlowFunction.apply _)

    implicit val ODFlowDataReads: Reads[ODFlowData] = (
      (JsPath \ "location").read[String] and
        (JsPath \ "PTflows").read[Vector[PTFlow]] and
        (JsPath \ "flows").read[Vector[PedestrianFlow]] and
        (JsPath \ "function_flows").read[Vector[PedestrianFlowFunction]]
      ) (ODFlowData.apply _)

    def readODFlowData(fileName: String): ODFlowData = {
      val source: BufferedSource = scala.io.Source.fromFile(fileName)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

      input.validate[ODFlowData] match {
        case s: JsSuccess[ODFlowData] => s.get
        case e: JsError => throw new Error("Error while parsing connectionMatrix    file: " + JsError.toJson(e).toString())
      }
    }


    case class TrackAccess(track: String, access: Vector[(String, Double)])

    implicit val TrackAccessReads: Reads[TrackAccess] = (
      (JsPath \ "track").read[String] and
        (JsPath \ "access").read[Vector[(String, Double)]]
      ) (TrackAccess.apply _)

    case class TrainTypeSplitFractions(trainType: String, capacity: Int, influence: Vector[TrackAccess])

    implicit val TrainTypeSplitFractionsReads: Reads[TrainTypeSplitFractions] = (
      (JsPath \ "type").read[String] and
        (JsPath \ "capacity").read[Int] and
        (JsPath \ "influence").read[Vector[TrackAccess]]
      ) (TrainTypeSplitFractions.apply _)

    case class SplitFractions(fractions: Vector[TrainTypeSplitFractions])

    implicit val SplitFractionsReads: Reads[SplitFractions] =
      (__ \ "train-type-influence").read[Vector[TrainTypeSplitFractions]].map { i => SplitFractions(i) }


    /* ----------------------------------------------------------------------------------
                                  TRAIN INDUCED FLOWS (TINF)
    -----------------------------------------------------------------------------------*/
    // assuming uniform access distribution
    def splitFractionsUniform(arrNodes: Iterable[Rectangle], depNodes: Iterable[Rectangle], totalFlow: Double): Iterable[(Rectangle, Rectangle, Double)] = {
      val perm = for {// all permutations of two lists of nodes
        a <- arrNodes
        b <- depNodes
        if b != a
      } yield {
        (a, b)
      }
      perm.map(p => (p._1, p._2, totalFlow / perm.size)) // split fractions over all permutations. Not realistic but a start
    }

    def splitFractionsUniform(arrNodes: Iterable[Rectangle], depNodes: Iterable[Rectangle]): Iterable[(Rectangle, Rectangle, Double)] = {
      val perm = for {// all permutations of two lists of nodes
        a <- arrNodes
        b <- depNodes
        if b != a
      } yield {
        (a, b)
      }
      perm.map(p => (p._1, p._2, 1.0 / perm.size)) // split fractions over all permutations. Not realistic but a start
    }


    /* ----------------------------------------------------------------------------------
                              DISAGRGEGATE PEDESTRIAN DEMAND
    -----------------------------------------------------------------------------------*/

    case class Pedestrian_JSON(
                                ID: String,
                                oZone: String,
                                dZone: String,
                                entryTime: Double,
                                exitTime: Double
                              )

    implicit val PedestrianJSONReads: Reads[Pedestrian_JSON] = (
      (JsPath \ "ID").read[String] and
        (JsPath \ "O").read[String] and
        (JsPath \ "D").read[String] and
        (JsPath \ "entryTime").read[Double] and
        (JsPath \ "exitTime").read[Double]
      ) (Pedestrian_JSON.apply _)


    /** Reads the PT schedule which is formatted as:
      *
      * {{{
      *   {
      *   "location": "lausanne",
      *   "trains": [
      *     {
      *       "id": "12217",
      *       "type": "S21",
      *       "track": 3,
      *       "arrival-time": "25320",
      *       "departure-time": "25380",
      *       "capacity": 515
      *     },
      *     {
      *       "id": "12218",
      *       "type": "S2",
      *       "track": 4,
      *       "arrival-time": "25600",
      *       "departure-time": "25650",
      *       "capacity": 517
      *     }
      *     }
      * }}}
      *
      * The vehicle movements are stored as [[hubmodel.io.input.JSONReaders.Vehicle_JSON]].
      *
      *
      * @param fileName
      * @return
      */
    def readSchedule(fileName: String): PublicTransportSchedule = {

      val source: BufferedSource = scala.io.Source.fromFile(fileName)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

      input.validate[PublicTransportScheduleReader] match {
        case s: JsSuccess[PublicTransportScheduleReader] => new PublicTransportSchedule(s.get.loc, s.get._timeTableInput.map(v => new Vehicle(TrainID_New(v.ID, ""), v.trainType, StopID_New(v.track, ""), v.arr, v.dep, v.capacity)))
        case e: JsError => throw new Error("Error while parsing train timetable: " + JsError.toJson(e).toString())
      }
    }

    /** Reads the TRANS-FORM public transport schedule which is formatted as:
      *
      * {{{
      *   {
      *   "location": "lausanne",
      *   "trains": [
      *     {
      *       "stop_id": "20212022",
      *       "trip_id": "60101",
      *       "arrival_time": "1314.12",
      *       "departure_time": "1339.57"
      *     },
      *     {
      *       "stop_id": "20212022",
      *       "trip_id": "20101",
      *       "arrival_time": "1526.64",
      *       "departure_time": "1555.86"
      *     }
      *     }
      *  }}}
      *
      * This format is used for the TRANS-FORM project. The vehicle movements are stored as [[Vehicle_JSON_TF]].
      *
      * @param fileName name of the file containing the JSON
      * @return collection of PT vehicle movements
      */
    def readScheduleTF(fileName: String): PublicTransportSchedule = {

      val source: BufferedSource = scala.io.Source.fromFile(fileName)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

      input.validate[PublicTransportScheduleReaderTF] match {
        case s: JsSuccess[PublicTransportScheduleReaderTF] => new PublicTransportSchedule(s.get.loc, s.get._timeTableInput.map(v => new Vehicle(TrainID_New(v.ID, ""), v.ID, StopID_New(v.stopID, ""), Some(v.arr), Some(v.dep), -1)))
        case e: JsError => throw new Error("Error while parsing train timetable file: " + fileName + "\n" + JsError.toJson(e).toString())
      }
    }

    def readDisaggDemand(fileName: String): Vector[(String, String, Option[Time])] = {

      val source: BufferedSource = scala.io.Source.fromFile(fileName)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

      input.validate[Vector[Pedestrian_JSON]] match {
        case s: JsSuccess[Vector[Pedestrian_JSON]] => s.get.map(p => (p.oZone, p.dZone, Some(Time(p.entryTime))))
        case e: JsError => throw new Error("Error while parsing disaggregate pedestrian: " + JsError.toJson(e).toString())
      }
    }

    def readDisaggDemandTF(fileName: String): Vector[(String, String, Option[Time])] = {

      val source: BufferedSource = scala.io.Source.fromFile(fileName)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

      input.validate[PedestrianCollectionReaderTF] match {
        case s: JsSuccess[PedestrianCollectionReaderTF] => s.get.population.map(p => (p.oZone, p.dZone, if (p.oTime.isDefined) {
          Some(Time(p.oTime.get))
        } else {
          None
        }))
        case e: JsError => throw new Error("Error while parsing disaggregate pedestrian for TF: " + JsError.toJson(e).toString())
      }
    }

    def readPedestrianFlows(file: String): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = {

      val _pedestrianFlowData: ODFlowData = {
        val source: BufferedSource = scala.io.Source.fromFile(file)
        val input: JsValue = Json.parse(try source.mkString finally source.close)

        input.validate[ODFlowData] match {
          case s: JsSuccess[ODFlowData] => s.get
          case e: JsError => throw new Error("Error while parsing OF flow file: " + JsError.toJson(e).toString())
        }
      }


      abstract class PedestrianFlowFunction(val start: Time, val end: Time)

      object PedestrianFlowFunctionOrdering extends Ordering[PedestrianFlowFunction] {
        def compare(a: PedestrianFlowFunction, b: PedestrianFlowFunction): Int = hubmodel.tools.TimeNumeric.compare(a.start, b.start)
      }

      case class SinPedestrianFlow(s: Time, e: Time, periodStretch: Double, periodShift: Double, a: Double, b: Double, c: Double, maxFlow: Double) extends PedestrianFlowFunction(s, e)


      case class LinearPedestrianFlow(s: Time, e: Time, rateAtStart: Double, rateAtEnd: Double, slope: Double) extends PedestrianFlowFunction(s, e) {
        if ((rateAtStart + slope * (this.end - this.start).value.toDouble - rateAtEnd).abs > math.pow(10, -5)) {
          throw new IllegalArgumentException("Flow rate at end doesn't match computed flow rate ! " + (rateAtStart + slope * (this.end - this.start).value) + " != " + rateAtEnd)
        }
      }

      case class ConstantPedestrianFlow(s: Time, e: Time, rate: Double) extends PedestrianFlowFunction(s, e) {
        if (rate < 0.0) {
          throw new IllegalArgumentException("Negative pedestrian flow rate !")
        }
      }


      val flowsFunction: Iterable[PedestrianFlowFunction_New] = {
        _pedestrianFlowData.functionalFlows.map(funcFlow => {
          val functions: Iterable[PedestrianFlowFunction] = (
            funcFlow.constantFunctions.map(cf => ConstantPedestrianFlow(Time(cf.start.toSecondOfDay), Time(cf.end.toSecondOfDay), cf.rate)) ++
              funcFlow.linearFunctions.map(lf => LinearPedestrianFlow(Time(lf.start.toSecondOfDay), Time(lf.end.toSecondOfDay), lf.rateAtStart, lf.rateAtEnd, lf.slope)) ++
              funcFlow.sinusFunctions.map(sf => SinPedestrianFlow(Time(sf.start.toSecondOfDay), Time(sf.end.toSecondOfDay), sf.periodStretch, sf.periodShift, sf.a, sf.b, sf.c, sf.maxFlow))
            ).sorted(PedestrianFlowFunctionOrdering)

          if (!functions.dropRight(1).zip(functions.tail).forall(pair => pair._1.end == pair._2.start)) {
            throw new IllegalArgumentException("Gaps in flow functions !")
          }

          def flowRateFunction(t: Time): Double = {
            functions.find(f => f.start <= t && t <= f.end) match {
              case Some(f) => f match {
                case l: LinearPedestrianFlow => {
                  l.rateAtStart + (t - l.start).value.toDouble * l.slope
                }
                case c: ConstantPedestrianFlow => {
                  c.rate
                }
                case s: SinPedestrianFlow => {
                  s.maxFlow * ((math.sin(t.value.toDouble * s.periodStretch + math.Pi * s.periodShift) + s.a) * s.b + s.c)
                }
                case err => throw new NotImplementedError("This type of flow function is not implemented !")
              }
              case other => 0.0
            }
          }

          val start: Time = Time((funcFlow.constantFunctions.map(_.start) ++ funcFlow.linearFunctions.map(_.start) ++ funcFlow.sinusFunctions.map(_.start)).min.toSecondOfDay)
          val end: Time = Time((funcFlow.constantFunctions.map(_.end) ++ funcFlow.linearFunctions.map(_.end) ++ funcFlow.sinusFunctions.map(_.end)).max.toSecondOfDay)

          PedestrianFlowFunction_New(NodeID_New(funcFlow.O, funcFlow.O.toString), NodeID_New(funcFlow.D, funcFlow.D.toString), start, end, flowRateFunction)

        })
      }

      (
        _pedestrianFlowData.flows.map(f => PedestrianFlow_New(NodeID_New(f.O, f.O.toString), NodeID_New(f.D, f.D.toString), f.start, f.end, f.f)),
        _pedestrianFlowData.PTflows.map(f => {
          PedestrianFlowPT_New(f.origin, f.destination, f.f)
        }),
        flowsFunction
      )
    }

    // closing Demand package


    /*abstract class InputFile{val name: String}
    case class TRANSFORMDemandFile(name: String) extends InputFile
    case class DemandFile(name: String) extends InputFile

    type FlowFile = InputFile
    type TimeTableFile = InputFile*/

    abstract class DemandData(val dir: Path){
      val flowFile: Path
      val timetableFile: Path
    }

    case class TRANSFORMDemandSet(private val flow: Path, private val timetable: Path, private val directory: String) extends DemandData(Paths.get(directory)) {
      val flowFile: Path = Paths.get(dir + "/" + flow.getFileName.toString)
      val timetableFile: Path = Paths.get(dir + "/" + timetable.getFileName.toString)
    }

    case class DemandSet(private val flow: Path, private val timetable: Path, private val directory: String) extends DemandData(Paths.get(directory)) {
      val flowFile: Path = Paths.get(dir + "/" + flow.getFileName.toString)
      val timetableFile: Path = Paths.get(dir + "/" + timetable.getFileName.toString)
    }

    type AggregateFlows = (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New])

    /** Reads the different files specifying the pedestrian demand
      *
      * @param config
      * @return
      */
    def readDemandSets(config: Config): Option[Seq[DemandData]] = {

      /** Lists all files from the directory "demand_sets" from the config file.
        *
        * @return
        */
      def getFilesInDirectory: Vector[Path] = {
        val multipleDemandStream: DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(config.getString("files.demand_sets")), "*.json")
        val files: Vector[Path] = multipleDemandStream.asScala.toVector
        multipleDemandStream.close()
        files
      }

      def getMatchingFlowTimeTable(files: Vector[Path]): Seq[(Path, Path)] = {

        // get the base name of the scenarios by removing the extension from the names in the config.
        val flowBaseName: String = Paths.get(config.getString("files.disaggregate_demand")).getFileName.toString.replace(".json", "")
        val timetableBaseName: String = Paths.get(config.getString("files.timetable")).getFileName.toString.replace(".json", "")


        if (files.size % 2 != 0) {
          throw new IllegalSimulationInput("Uneven number of files for multiple demand sets ! (" + files.size + " files found)")
        } else if (files.isEmpty) {
          throw new IllegalSimulationInput("No files for multiple demand sets !")
        } else if (files.size == 2) {
          println("Warning ! Only one set of demands used for the multiple demand inputs. ")
          Seq((
            files.find(_.getFileName.toString.contains(flowBaseName)).get,
            files.find(_.getFileName.toString.contains(timetableBaseName)).get
          )
          )
        } else {
          files
            .groupBy(f => f.getFileName.getFileName.toString.split("_").last.replace(".json", ""))
            .map(grouped => (grouped._2.find(_.getFileName.toString.contains(flowBaseName)).get, grouped._2.find(_.getFileName.toString.contains(timetableBaseName)).get)).toVector
        }
      }


      // Check if the input parameters makes sense
      if (config.getBoolean("files.multiple_demand_sets") && config.getBoolean("files.multiple_demand_sets_TF")) {
        throw new IllegalSimulationInput("Multiple demand sets for standard and TRANS-FORM cannot be set together !")
      }

      val files = getFilesInDirectory
      val demandFiles: Seq[(Path, Path)] = getMatchingFlowTimeTable(files)

      if (config.getBoolean("files.multiple_demand_sets_TF")) {Some(demandFiles.map(ss => TRANSFORMDemandSet(ss._1, ss._2, config.getString("files.demand_sets"))))}
      else if (config.getBoolean("files.multiple_demand_sets")) { Some(demandFiles.map(ss => DemandSet(ss._1, ss._2, config.getString("files.demand_sets"))))}
      else {None}
    }

  } // closing HubModel.HubInput package
}
