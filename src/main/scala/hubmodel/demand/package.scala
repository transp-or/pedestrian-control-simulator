import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.io.BufferedSource

//import pedtrack.{Time, StringImprovements}
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, LocalTime}

import hubmodel.supply.NodeID


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

    implicit val trainReads: Reads[Train] = (
      (JsPath \ "id").read[String](minLength[String](1)) and
        (JsPath \ "type").read[String] and
        (JsPath \ "track").read[Int](min(0)) and
        (JsPath \ "arrival-time").readNullable[LocalTime] and
        (JsPath \ "departure-time").readNullable[LocalTime] and
        (JsPath \ "capacity").read[Int](min(0))
      ) (Train.apply _)

    implicit val trainTimeTableReads: Reads[TrainTimeTable] = (
      (JsPath \ "location").read[String](minLength[String](1)) and
        (JsPath \ "trains").read[Vector[Train]]
      ) (TrainTimeTable.apply _)

    implicit val Track2NodesReads: Reads[Track2Nodes] = (
      (JsPath \ "track").read[Int] and
        (JsPath \ "nodes").read[Vector[NodeID]]
      ) (Track2Nodes.apply _)

    implicit val track2nodeMappingReads: Reads[Track2NodeMapping] = (
      (JsPath \ "location").read[String] and
        (JsPath \ "track2nodes").read[Vector[Track2Nodes]]
      ) (Track2NodeMapping.apply _)


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


    implicit val ODFlowDataReads: Reads[ODFlowData] = (
      (JsPath \ "location").read[String] and
        (JsPath \ "PTflows").read[Vector[PTFlow]] and
        (JsPath \ "flows").read[Vector[PedestrianFlow]]
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
    def splitFractionsUniform(arrNodes: Iterable[VertexRectangle], depNodes: Iterable[VertexRectangle], totalFlow: Double): Iterable[(VertexRectangle, VertexRectangle, Double)] = {
      val perm = for {// all permutations of two lists of nodes
        a <- arrNodes
        b <- depNodes
        if b != a
      } yield {
        (a, b)
      }
      perm.map(p => (p._1, p._2, totalFlow / perm.size)) // split fractions over all permutations. Not realistic but a start
    }


    /* ----------------------------------------------------------------------------------
                              DISAGRGEGATE PEDESTRIAN DEMAND
    -----------------------------------------------------------------------------------*/

    case class PedestrianJSON(
                             ID: String,
                             oZone: String,
                             dZone: String,
                             entryTime: Double,
                             exitTime: Double
                             )

    implicit val PedestrianJSONReads: Reads[PedestrianJSON] = (
      (JsPath \ "ID").read[String] and
        (JsPath \ "O").read[String] and
        (JsPath \ "D").read[String] and
        (JsPath \ "entryTime").read[Double] and
        (JsPath \ "exitTime").read[Double]
      ) (PedestrianJSON.apply _)

    // closing Demand package
  }

  // closing HubModel.HubInput package
}
