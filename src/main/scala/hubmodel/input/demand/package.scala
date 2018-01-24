import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.io.BufferedSource

//import pedtrack.{Time, StringImprovements}
import hubmodel.input.infrastructure.{NodeID, InfraODModel}

import hubmodel.input.infrastructure.TrackID


/**
  * Created by nicholas on 3/8/17.
  */
package hubmodel.input {

  import java.time.{LocalDateTime, LocalTime}
  import java.time.format.DateTimeFormatter

  import hubmodel.input.infrastructure.TrainID

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
      (JsPath \ "origin").read[NodeID] and
        (JsPath \ "destination").read[NodeID] and
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
    val splitFractions: Iterable[NodeID] => Vector[Double] = nodes => Vector.fill(nodes.size) {
      1.0 / nodes.size
    }

    // assumes passenger use infrastructure to capacity


    /*def timeTable2Inflow(infra: InfraODModel, trainSchedule: TrainTimeTable, trainPassengers: Vector[PassengerFlow]): Vector[PedestrianFlow] = {
      def connectionsToSplitFractionsHelper(arrNodes: Iterable[NodeID], depNodes: Iterable[NodeID]): Vector[(NodeID, NodeID, Double)] = {
        val perm = for { // all permutations of two lists of nodes
          a <- arrNodes
          b <- depNodes
          if b != a
        } yield {(a,b)}
        perm.map(p => (p._1, p._2, 1.0/perm.size)).toVector // split fractions over all permutations. Not realistic but a start
      }

      /** Vector of required connections extracted from the passenger flows */
      val requiredConnections: Vector[(TrainID, TrainID)] = trainPassengers.map(p => (p.O, p.D))

      /** Map from connecting trains to corresponding nodes */
      val connectionsToNodes: Map[(TrainID, TrainID),(Iterable[NodeID], Iterable[NodeID])] = requiredConnections.map(c => c -> (infra.platform2Node(infra.track2platform(trainSchedule.train2Track(c._1))), infra.platform2Node(infra.track2platform(trainSchedule.train2Track(c._2))))).toMap

      /** Train connections to split fractions map */
      val connectionsToSplitFractions: Map[(TrainID, TrainID),Vector[(NodeID, NodeID, Double)]] = connectionsToNodes.map(c => c._1 -> connectionsToSplitFractionsHelper(c._2._1, c._2._2))
      println(connectionsToSplitFractions)
      trainPassengers
        .filter(PTFlow => !infra.isOnSamePlatform(trainSchedule.train2Track(PTFlow.O), trainSchedule.train2Track(PTFlow.D)))
        .flatMap(p => {
        connectionsToSplitFractions(p.O, p.D).map(c =>
          PedestrianFlow(c._1, c._2, trainSchedule.timeTable(p.O).arr, trainSchedule.timeTable(p.O).arr.plusSeconds(180), p.flow*c._3)
        ) // very strong assumption: alighting happens in 3 minutes. Need to relax this later on
      }
      )
    }*/

    // closing Demand package
  }

  // closing HubModel.HubInput package
}
