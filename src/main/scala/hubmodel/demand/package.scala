import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.io.BufferedSource

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, LocalTime}
import scala.collection.JavaConversions._

import java.nio.file.{DirectoryStream, Files, Path, Paths}

import com.typesafe.config.Config
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.demand.transit.Vehicle
import hubmodel.input.JSONReaders.PublicTransportScheduleReader
import hubmodel.input.JSONReaders.TRANSFORM.{PedestrianCollectionReaderTF, PublicTransportScheduleReaderTF}
import hubmodel.supply.{NodeID_New, StopID_New, TrainID_New}
import hubmodel.tools.IllegalSimulationInput
import hubmodel.tools.cells.Rectangle

/**
  * Created by nicholas on 3/8/17.
  */
package hubmodel {



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


    def readSchedule(fileName: String): PublicTransportSchedule = {

      val source: BufferedSource = scala.io.Source.fromFile(fileName)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

      input.validate[PublicTransportScheduleReader] match {
        case s: JsSuccess[PublicTransportScheduleReader] => new PublicTransportSchedule(s.get.loc, s.get._timeTableInput.map(v => new Vehicle(TrainID_New(v.ID, ""), v.trainType, StopID_New(v.track, ""), v.arr, v.dep, v.capacity)))
        case e: JsError => throw new Error("Error while parsing train timetable: " + JsError.toJson(e).toString())
      }
    }

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
        case s: JsSuccess[PedestrianCollectionReaderTF] => s.get.population.map(p => (p.oZone, p.dZone, if (p.oTime.isDefined) { Some(Time(p.oTime.get)) } else { None }))
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
        def compare(a: PedestrianFlowFunction, b: PedestrianFlowFunction): Int = hubmodel.TimeNumeric.compare(a.start, b.start)
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


    def readDemandSets(config: Config): Option[Seq[(String, String)]] = {

      if (config.getBoolean("sim.read_multiple_TF_demand_sets")) {

        if (!((Paths.get(config.getString("files.TF_demand_sets")).toString == Paths.get(config.getString("files.flows_TF")).getParent.toString) &&
          (Paths.get(config.getString("files.flows_TF")).getParent.toString == Paths.get(config.getString("files.timetable_TF")).getParent.toString))) {
          throw new IllegalSimulationInput("Directories for multiple demand sets do not match !")
        }

        val multipleDemandStream: DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(config.getString("files.TF_demand_sets")), "*.json")

        val files: Vector[Path] = multipleDemandStream.toVector

        multipleDemandStream.close()

        val flowBaseName: String = Paths.get(config.getString("files.flows_TF")).getFileName.toString.replace(".json", "")
        val timetableBaseName: String = Paths.get(config.getString("files.timetable_TF")).getFileName.toString.replace(".json", "")

        try {
          if (files.size % 2 != 0) {
            throw new IllegalSimulationInput("Uneven number of files for multiple demand sets ! (" + files.size + " files found)")
          } else if (files.isEmpty) {
            throw new IllegalSimulationInput("No files for multiple demand sets !")
          } else if (files.size == 2) {
            println("Warning ! Only one set of demands used for the multiple demand inputs. ")
            Some(
              Seq((
                files.find(_.getFileName.toString.contains(flowBaseName)).get.toString,
                files.find(_.getFileName.toString.contains(timetableBaseName)).get.toString
              )
              )
            )
          } else {
            Some(
              files
                .groupBy(f => f.getFileName.getFileName.toString.split("_").last.replace(".json", ""))
                .map(grouped => (grouped._2.find(_.getFileName.toString.contains(flowBaseName)).get.toString, grouped._2.find(_.getFileName.toString.contains(timetableBaseName)).get.toString)).toVector
            )
          }
        } catch {
          case e: Exception => throw e
        }
      } else {
        None
      }
    }

  } // closing HubModel.HubInput package
}
