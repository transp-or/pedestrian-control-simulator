package hubmodel.input {

  /**
    * Created by nicholas on 3/6/17.
    */

  import hubmodel.Position
  import play.api.libs.functional.syntax._
  import play.api.libs.json.Reads.{min, minLength}
  import play.api.libs.json._

  /** This package contains the classes and methods which are needed for building
    *
    */
  package object infrastructure {
    abstract class NodeParent
    case class NodeID_New(ID: Int, humanID: String) extends NodeParent
    case class TrackID_New(ID: Int, humanID: String) extends NodeParent
    case class TrainID_New(ID: String, humanID: String) extends NodeParent

    type NodeID = Int
    type TrackID = Int
    type TrainID = String
    type ODID = String


    /* ----------------------------------------------------------------------------------
                             INFRASTRUCTURE TRAIT
  -----------------------------------------------------------------------------------*/

    /** Superclass for all infrastructure specifications
      *
      */
    trait Infrastructure {
      val location: String
      val subLocation: String
    }

    /* ----------------------------------------------------------------------------------
                          JSON parser for social force infrastructure
    -----------------------------------------------------------------------------------*/

    case class Doorway(comment: String, x1: Double, y1: Double, x2: Double, y2: Double){
      val startPoint: Position = breeze.linalg.DenseVector(x1, y1)
      val endPoint: Position = breeze.linalg.DenseVector(x2, y2)
    }

    /** Reader for vector for a single Walls
      *
      */
    implicit val DoorwayReads: Reads[Doorway] = (
      (JsPath \ "comment").read[String] and
        (JsPath \ "x1").read[Double] and
        (JsPath \ "y1").read[Double] and
        (JsPath \ "x2").read[Double] and
        (JsPath \ "y2").read[Double]
      ) (Doorway.apply _)

    /** Writer for a single Wall
      *
      */
    implicit val DoorwayWrites: Writes[Doorway] = (
      (JsPath \ "comment").write[String] and
        (JsPath \ "x1").write[Double] and
        (JsPath \ "y1").write[Double] and
        (JsPath \ "x2").write[Double] and
        (JsPath \ "y2").write[Double]
      ) (unlift(Doorway.unapply))

    /** Wall class for interactions with pedestrians
      *
      * @param x1 x coord of first point
      * @param y1 y coord of first point
      * @param x2 x coord of second point
      * @param y2 y coord of second point
      */
    case class Wall(comment: String, x1: Double, y1: Double, x2: Double, y2: Double){
      val startPoint: Position = breeze.linalg.DenseVector(x1, y1)
      val endPoint: Position = breeze.linalg.DenseVector(x2, y2)
    }

    /** Reader for vector for a single Walls
      *
      */
    implicit val WallReads: Reads[Wall] = (
      (JsPath \ "comment").read[String] and
      (JsPath \ "x1").read[Double] and
        (JsPath \ "y1").read[Double] and
        (JsPath \ "x2").read[Double] and
        (JsPath \ "y2").read[Double]
      ) (Wall.apply _)

    /** Writer for a single Wall
      *
      */
    implicit val WallWrites: Writes[Wall] = (
      (JsPath \ "comment").write[String] and
      (JsPath \ "x1").write[Double] and
        (JsPath \ "y1").write[Double] and
        (JsPath \ "x2").write[Double] and
        (JsPath \ "y2").write[Double]
      ) (unlift(Wall.unapply))

    /** For reading JSON files storing the specs
      *
      * @param location main location
      * @param subLocation subarea
      * @param walls vector storing the walls
      */
    case class InfraSFParser(location: String, subLocation: String, walls: Vector[Wall]) extends Infrastructure
    implicit val InfraSFParserWrites: Writes[InfraSFParser] = (
      (JsPath \ "location").write[String] and
        (JsPath \ "sublocation").write[String] and
        (JsPath \ "walls").write[Vector[Wall]]
      ) (unlift(InfraSFParser.unapply))

    /** Wrtier for SF infrastructure specifications
      *
      */
    implicit val InfraSFParserReads: Reads[InfraSFParser] = (
      (JsPath \ "location").read[String](minLength[String](2)) and
        (JsPath \ "sublocation").read[String](minLength[String](2)) and
        (JsPath \ "walls").read[Vector[Wall]]
      ) (InfraSFParser.apply _)

    /** For reading JSON files storing the specs
      *
      * @param location main location
      * @param subLocation subarea
      * @param walls vector storing the walls
      */
    case class InfraSFParserWithDoor(location: String, subLocation: String, walls: Vector[Wall], doors: Vector[Doorway]) extends Infrastructure
    implicit val InfraSFParserWithDoorWrites: Writes[InfraSFParserWithDoor] = (
      (JsPath \ "location").write[String] and
        (JsPath \ "sublocation").write[String] and
        (JsPath \ "walls").write[Vector[Wall]] and
        (JsPath \ "doorways").write[Vector[Doorway]]
      ) (unlift(InfraSFParserWithDoor.unapply))

    /** Wrtier for SF infrastructure specifications
      *
      */
    implicit val InfraSFParserWithDoorReads: Reads[InfraSFParserWithDoor] = (
      (JsPath \ "location").read[String](minLength[String](2)) and
        (JsPath \ "sublocation").read[String](minLength[String](2)) and
        (JsPath \ "walls").read[Vector[Wall]] and
        (JsPath \ "doorways").read[Vector[Doorway]]
      ) (InfraSFParserWithDoor.apply _)


    /* ----------------------------------------------------------------------------------
                      JSON parser for graph infrastructure used with SF
    -----------------------------------------------------------------------------------*/

    case class NodeRouteGraph_JSON(name: String, x: Double, y: Double, x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double, x4: Double, y4: Double)

    implicit val NodeRouteGraph_JSONWrites: Writes[NodeRouteGraph_JSON] = (
      (JsPath \ "name").write[String] and
        (JsPath \ "x").write[Double] and
        (JsPath \ "y").write[Double] and
        (JsPath \ "x1").write[Double] and
        (JsPath \ "y1").write[Double] and
        (JsPath \ "x2").write[Double] and
        (JsPath \ "y2").write[Double] and
        (JsPath \ "x3").write[Double] and
        (JsPath \ "y3").write[Double] and
        (JsPath \ "x4").write[Double] and
        (JsPath \ "y4").write[Double]
      ) (unlift(NodeRouteGraph_JSON.unapply))

    /** Wrtier for SF infrastructure specifications
      *
      */
    implicit val NodeRouteGraph_JSONReads: Reads[NodeRouteGraph_JSON] = (
      (JsPath \ "name").read[String] and
        (JsPath \ "x").read[Double] and
        (JsPath \ "y").read[Double] and
        (JsPath \ "x1").read[Double] and
        (JsPath \ "y1").read[Double] and
        (JsPath \ "x2").read[Double] and
        (JsPath \ "y2").read[Double] and
        (JsPath \ "x3").read[Double] and
        (JsPath \ "y3").read[Double] and
        (JsPath \ "x4").read[Double] and
        (JsPath \ "y4").read[Double]
      ) (NodeRouteGraph_JSON.apply _)



    case class Connectivity_JSON(node: String, conn: List[String])
    implicit val Connectivity_JSONWrites: Writes[Connectivity_JSON] = (
      (JsPath \ "node").write[String] and
        (JsPath \ "connected_to").write[List[String]]
      ) (unlift(Connectivity_JSON.unapply))

    implicit val Connectivity_JSONReads: Reads[Connectivity_JSON] = (
      (JsPath \ "node").read[String] and
        (JsPath \ "connected_to").read[List[String]]
      ) (Connectivity_JSON.apply _)

    case class FlowGates_JSON(o: String, d: String, start_pos_x: Double, start_pos_y: Double, end_pos_x: Double, end_pos_y: Double)

    implicit val FlowGates_JSONWrites: Writes[FlowGates_JSON] = (
      (JsPath \ "o").write[String] and
        (JsPath \ "d").write[String] and
        (JsPath \ "start_pos_x").write[Double] and
        (JsPath \ "start_pos_y").write[Double] and
        (JsPath \ "end_pos_x").write[Double] and
        (JsPath \ "end_pos_y").write[Double]
      ) (unlift(FlowGates_JSON.unapply))

    implicit val FlowGates_JSONReads: Reads[FlowGates_JSON] = (
      (JsPath \ "o").read[String] and
        (JsPath \ "d").read[String] and
        (JsPath \ "start_pos_x").read[Double] and
        (JsPath \ "start_pos_y").read[Double] and
        (JsPath \ "end_pos_x").read[Double] and
        (JsPath \ "end_pos_y").read[Double]
      ) (FlowGates_JSON.apply _)

    case class MovingWalkways_JSON(o: String, d: String)

    implicit val MovingWalkways_JSONWrites: Writes[MovingWalkways_JSON] = (
      (JsPath \ "o").write[String] and
        (JsPath \ "d").write[String]
      ) (unlift(MovingWalkways_JSON.unapply))

    implicit val MovingWalkways_JSONReads: Reads[MovingWalkways_JSON] = (
      (JsPath \ "o").read[String] and
        (JsPath \ "d").read[String]
      ) (MovingWalkways_JSON.apply _)

    case class BinaryGates_JSON(o: String, d: String, inflow_to: String, s_x: Double, s_y: Double, e_x: Double, e_y: Double)

    implicit val BinaryGates_JSONWrites: Writes[BinaryGates_JSON] = (
      (JsPath \ "o").write[String] and
        (JsPath \ "d").write[String] and
        (JsPath \ "inflow_to").write[String] and
        (JsPath \ "startPos_x").write[Double] and
        (JsPath \ "startPos_y").write[Double] and
        (JsPath \ "endPos_x").write[Double] and
        (JsPath \ "endPos_y").write[Double]
      ) (unlift(BinaryGates_JSON.unapply))

    implicit val BinaryGates_JSONReads: Reads[BinaryGates_JSON] = (
      (JsPath \ "o").read[String] and
        (JsPath \ "d").read[String] and
        (JsPath \ "inflow_to").read[String] and
        (JsPath \ "startPos_x").read[Double] and
        (JsPath \ "startPos_y").read[Double] and
        (JsPath \ "endPos_x").read[Double] and
        (JsPath \ "endPos_y").read[Double]
      ) (BinaryGates_JSON.apply _)


    case class InfraGraphParser(location: String,
                                subLocation: String,
                                nodes: Vector[NodeRouteGraph_JSON],
                                standardConnections: Vector[Connectivity_JSON],
                                flowGates: Vector[FlowGates_JSON],
                                binaryGates: Vector[BinaryGates_JSON],
                                movingWalkways: Vector[MovingWalkways_JSON]) extends Infrastructure

    /** Reader for SF infrastructure specification
      *
      */
    implicit val InfraGraphParserWrites: Writes[InfraGraphParser] = (
      (JsPath \ "location").write[String] and
        (JsPath \ "sublocation").write[String] and
        (JsPath \ "nodes").write[Vector[NodeRouteGraph_JSON]] and
        (JsPath \ "connectivity").write[Vector[Connectivity_JSON]] and
        (JsPath \ "flow_gates").write[Vector[FlowGates_JSON]] and
        (JsPath \ "binary_gates").write[Vector[BinaryGates_JSON]] and
        (JsPath \ "moving_walkways").write[Vector[MovingWalkways_JSON]]
      ) (unlift(InfraGraphParser.unapply))

    /** Writer for SF infrastructure specifications
      *
      */
    implicit val InfraGraphParserReads: Reads[InfraGraphParser] = (
      (JsPath \ "location").read[String](minLength[String](2)) and
        (JsPath \ "sublocation").read[String](minLength[String](2)) and
        (JsPath \ "nodes").read[Vector[NodeRouteGraph_JSON]] and
        (JsPath \ "connectivity").read[Vector[Connectivity_JSON]] and
        (JsPath \ "flow_gates").read[Vector[FlowGates_JSON]] and
        (JsPath \ "binary_gates").read[Vector[BinaryGates_JSON]] and
        (JsPath \ "moving_walkways").read[Vector[MovingWalkways_JSON]]
      ) (InfraGraphParser.apply _)


    /* ----------------------------------------------------------------------------------
                        OD DISTANCE INFRASTRUCTURE SPECIFICATION
    -----------------------------------------------------------------------------------*/


    // ------------ Node to platform mapping -----------------//
    case class Node2PlatMapping(node: String, plat: String)
    implicit val MappingNodePlatReads: Reads[Node2PlatMapping] = (
      (JsPath \ "node").read[String] and
        (JsPath \ "platform").read[String]
      ) (Node2PlatMapping.apply _)
    implicit val MappingNodePlatWrites: Writes[Node2PlatMapping] = (
      (JsPath \ "node").write[String] and
        (JsPath \ "platform").write[String]
      ) (unlift(Node2PlatMapping.unapply))

    case class Platform2TrackMapping(platform: String, tracks: Vector[Int])
    implicit val MappingPlatTrackReads: Reads[Platform2TrackMapping] = (
      (JsPath \ "platform").read[String] and
        (JsPath \ "tracks").read[Vector[Int]]
      ) (Platform2TrackMapping.apply _)
    implicit val MappingPlatTrackWrites: Writes[Platform2TrackMapping] = (
      (JsPath \ "platform").write[String] and
        (JsPath \ "tracks").write[Vector[Int]]
      ) (unlift(Platform2TrackMapping.unapply))


    //object InfraODParser {
      implicit val InfraODWrites: Writes[InfraODParser] = (
        (JsPath \ "location").write[String] and
          (JsPath \ "sublocation").write[String] and
          (JsPath \ "od").write[Vector[ODPairWithoutCapacity]] and
          (JsPath \ "nodethroughput").write[Vector[NodeThroughput]] and
          (JsPath \ "nodeplatform").write[Vector[Node2PlatMapping]] and
          (JsPath \ "platformtrack").write[Vector[Platform2TrackMapping]]
        ) (unlift(InfraODParser.unapply))

      // reads the InfraOD object from JSON. This is the full network as an OD matrix
      implicit val InfraODReads: Reads[InfraODParser] = (
        (JsPath \ "location").read[String](minLength[String](2)) and
          (JsPath \ "sublocation").read[String](minLength[String](2)) and
          (JsPath \ "od").read[Vector[ODPairWithoutCapacity]] and
          (JsPath \ "nodethroughput").read[Vector[NodeThroughput]] and
          (JsPath \ "nodeplatform").read[Vector[Node2PlatMapping]] and
          (JsPath \ "platformtrack").read[Vector[Platform2TrackMapping]]
        ) (InfraODParser.apply _)

      // form https://gist.github.com/alexanderjarvis/4595298
      /*implicit def tuple2Writes[A, B](implicit a: Writes[A], b: Writes[B]): Writes[Tuple2[A, B]] = new Writes[Tuple2[A, B]] {
        def writes(tuple: Tuple2[A, B]) = JsArray(Seq(a.writes(tuple._1), b.writes(tuple._2)))
      }

      implicit def tuple2Reads[A, B](implicit aReads: Reads[A], bReads: Reads[B]): Reads[Tuple2[A, B]] = Reads[Tuple2[A, B]] {
        case JsArray(arr) if arr.size == 2 => for {
          a <- aReads.reads(arr(0))
          b <- bReads.reads(arr(1))
        } yield (a, b)
        case _ => JsError(Seq(JsPath() -> Seq(ValidationError("Expected array of two elements"))))
      }*/
    //}

    /* ----------------------------------------------------------------------------------
                                NODE THROUGHPUT CLASS
-----------------------------------------------------------------------------------*/
    case class NodeThroughput(ID: String, throughput: Double)

    object NodeThroughput {
      implicit val NodeThroughputReads: Reads[NodeThroughput] = (
        (JsPath \ "node").read[String] and
          (JsPath \ "cap").read[Double](min(0.0))
        ) (NodeThroughput.apply _)

      implicit val NodeThroughputWrites: Writes[NodeThroughput] = (
        (JsPath \ "node").write[String] and
          (JsPath \ "cap").write[Double]
        ) (unlift(NodeThroughput.unapply))
    }

    /* ----------------------------------------------------------------------------------
                              OD PAIR WITHOUT THROUGHPUT LIMITATIONS
-----------------------------------------------------------------------------------*/
    case class ODPairWithoutCapacity(O: String, D: String, distance: Double)

    // extends ODPair
    object ODPairWithoutCapacity {
      implicit val ODPairWithoutCapacityWrites: Writes[ODPairWithoutCapacity] = (
        (JsPath \ "o").write[String] and
          (JsPath \ "d").write[String] and
          (JsPath \ "distance").write[Double]
        ) (unlift(ODPairWithoutCapacity.unapply))

      // reads the od objects from JSON
      implicit val ODPairWithoutCapacityReads: Reads[ODPairWithoutCapacity] = (
        (JsPath \ "o").read[String] and
          (JsPath \ "d").read[String] and
          (JsPath \ "distance").read[Double]
        ) (ODPairWithoutCapacity.apply _)
    }

    /* ----------------------------------------------------------------------------------
                            OD PAIR
    -----------------------------------------------------------------------------------*/
    case class ODPair(O: String, D: String)

    // extends ODPair
    object ODPair {
      implicit val ODPairWrites: Writes[ODPair] = (
        (JsPath \ "o").write[String] and
          (JsPath \ "d").write[String]
        ) (unlift(ODPair.unapply))

      // reads the od objects from JSON
      implicit val ODPairReads: Reads[ODPair] = (
        (JsPath \ "o").read[String] and
          (JsPath \ "d").read[String]
        ) (ODPair.apply _)
    }

  }

}

