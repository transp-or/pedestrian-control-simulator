package hubmodel.input


import hubmodel.supply.{NodeIDOld, TrackIDOld}
import hubmodel.{Position, Time}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads.minLength
import play.api.libs.json._

package object JSONReaders {


  // ******************************************************************************************
  //                   CASE CLASSES AND IMPLICIT CONVERSIONS FOR DEMAND READER
  // ******************************************************************************************


  /** Train object. CAPACITY SHOULD BE CHANGED FOR A TRAIN TYPE AT SOME POINT
    *
    * @param ID       unique identifier (String)
    * @param track    track on which the train arrives
    * @param arr      arrival time
    * @param dep      departure time
    * @param capacity max capacity of the train
    */
  private[JSONReaders] case class Vehicle_JSON(ID: String, trainType: String, track: String, arr: Option[Time], dep: Option[Time], capacity: Int)

  implicit val trainReads: Reads[Vehicle_JSON] = (
    (JsPath \ "id").read[String](minLength[String](1)) and
      (JsPath \ "type").read[String] and
      (JsPath \ "track").read[String] and
      (JsPath \ "arrival-time").readNullable[Time] and
      (JsPath \ "departure-time").readNullable[Time] and
      (JsPath \ "capacity").read[Int]
    ) (Vehicle_JSON.apply _)

  /** Pairs of tracks and corresponding nodes
    *
    * @param stop  id of track, as [[TrackIDOld]]
    * @param nodes Vector of [[NodeIDOld]]
    */
  private[JSONReaders] case class Stop2Nodes_JSON(stop: String, nodes: Vector[NodeIDOld])

  private[JSONReaders] case class Zone2Nodes_JSON(zone: Int, nodes: Vector[NodeIDOld])


  implicit val Stop2NodesReads: Reads[Stop2Nodes_JSON] = (
    (JsPath \ "stop").read[String] and
      (JsPath \ "nodes").read[Vector[NodeIDOld]]
    ) (Stop2Nodes_JSON.apply _)

  implicit val Zone2NodesReads: Reads[Zone2Nodes_JSON] = (
    (JsPath \ "zone").read[Int] and
      (JsPath \ "nodes").read[Vector[NodeIDOld]]
    ) (Zone2Nodes_JSON.apply _)


  /** Container for the track to nodes mapping
    *
    * @param loc                    location of this map
    * @param Track2NodeMappingInput raw data
    */
  case class Track2NodeMapping_JSON(loc: String, Track2NodeMappingInput: Vector[Stop2Nodes_JSON], WalkingZones2NodeMappingInput: Vector[Zone2Nodes_JSON])

  implicit val track2nodeMappingReads: Reads[Track2NodeMapping_JSON] = (
    (JsPath \ "location").read[String] and
      (JsPath \ "stop2nodes").read[Vector[Stop2Nodes_JSON]] and
      (JsPath \ "zone2nodes").read[Vector[Zone2Nodes_JSON]]
    ) (Track2NodeMapping_JSON.apply _)


  // ******************************************************************************************
  //                   CASE CLASSES AND IMPLICIT CONVERSIONS FOR GRAPH READER
  // ******************************************************************************************

  /**
    * Rectangular zone used in the graph as a vertex. The four corners must be specified in the following order.
    * This object is also used for reading the monitored areas which are associated to gates.
    *
    * @param name human readable unique name of the node
    * @param x1   bottom left x-coord
    * @param y1   bottom left y-coord
    * @param x2   bottom right x-coord
    * @param y2   bottom right y-coord
    * @param x3   top right x-coord
    * @param y3   top right y-coord
    * @param x4   top left x-coord
    * @param y4   top left y-coord
    */
  private[JSONReaders] case class Vertex_JSON(name: String, x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double, x4: Double, y4: Double, OD: Boolean)

  /**
    * Reads a JSON structure into a [[Vertex_JSON]] object. No validation on the arguments is done.
    */
  implicit val Vertex_JSON_Reads: Reads[Vertex_JSON] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double] and
      (JsPath \ "x3").read[Double] and
      (JsPath \ "y3").read[Double] and
      (JsPath \ "x4").read[Double] and
      (JsPath \ "y4").read[Double] and
      (JsPath \ "OD").read[Boolean]
    ) (Vertex_JSON.apply _)


  /**
    * Connections leading from one specific vertex to all connected vertices.
    *
    * @param node node from which the connections leaves
    * @param conn collection of destination nodes
    */
  case class Connectivity_JSON(node: String, conn: List[String])

  /**
    * Reads the JSON structures into a [[Connectivity_JSON]] object. No validation is done.
    */
  implicit val Connectivity_JSONReads: Reads[Connectivity_JSON] = (
    (JsPath \ "node").read[String] and
      (JsPath \ "connected_to").read[List[String]]
    ) (Connectivity_JSON.apply _)


  /**
    * Connections leading from one specific vertex to all connected vertices.
    *
    * @param name     name of the alternate connection set
    * @param conn2Add collection of connections to add
    */
  private[JSONReaders] case class ConnectivityAlternatives_JSON(name: String, frac: Double, conn2Add: List[Connectivity_JSON], conn2Remove: List[Connectivity_JSON])

  /**
    * Reads the JSON structures into a [[Connectivity_JSON]] object. No validation is done.
    */
  implicit val ConnectivityAlternatives_JSONReads: Reads[ConnectivityAlternatives_JSON] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "fraction_of_pop").read[Double] and
      (JsPath \ "connectivity_to_add").read[List[Connectivity_JSON]] and
      (JsPath \ "connectivity_to_remove").read[List[Connectivity_JSON]]
    ) (ConnectivityAlternatives_JSON.apply _)

  /**
    * Object used for reading a flow gate from JSON. The arguments are the properties of the flow gate.
    *
    * @param o           origin vertex
    * @param d           destination vertex
    * @param start_pos_x x-coord of one physical end of the gate
    * @param start_pos_y y-coord of one physical end of the gate
    * @param end_pos_x   x-coord of other physical end of the gate
    * @param end_pos_y   y-coord of other physical end of the gate
    * @param area        area behind the gate which is monitored
    * @param funcForm    string giving the type of function form
    * @param funcParam   the parameters of the functional form
    */
  private[JSONReaders] case class FlowGates_JSON(o: String, d: String, start_pos_x: Double, start_pos_y: Double, end_pos_x: Double, end_pos_y: Double, area: String, funcForm: Option[String], funcParam: Option[Vector[Double]])

  /**
    * Reads the JSON structure into a [[FlowGates_JSON]] object. No validation on arguments is done.
    */
  implicit val FlowGates_JSONReads: Reads[FlowGates_JSON] = (
    (JsPath \ "o").read[String] and
      (JsPath \ "d").read[String] and
      (JsPath \ "start_pos_x").read[Double] and
      (JsPath \ "start_pos_y").read[Double] and
      (JsPath \ "end_pos_x").read[Double] and
      (JsPath \ "end_pos_y").read[Double] and
      (JsPath \ "controlled_area").read[String] and
      (JsPath \ "functional_form").readNullable[String] and
      (JsPath \ "functional_parameters").readNullable[Vector[Double]]
    ) (FlowGates_JSON.apply _)

  /**
    * Moving walkway object. Currently not used by the simulation.
    *
    * @param o origin of the moving walkway
    * @param d destination of the moving walkway
    */
  private[JSONReaders] case class MovingWalkways_JSON(o: String, d: String)

  /**
    * Reads the JSON structure into a [[MovingWalkways_JSON]] object. No validation on arguments is done.
    */
  implicit val MovingWalkways_JSONReads: Reads[MovingWalkways_JSON] = (
    (JsPath \ "o").read[String] and
      (JsPath \ "d").read[String]
    ) (MovingWalkways_JSON.apply _)

  /**
    * Line through which the flow of pedestrians is calculated.
    * IMPORTANT: the definition of inflow is the following: a person is entering the zone behind the line if he crosses
    * from left to right when standing in point (x1,y1). Therefore when creating these flow lines the order of the
    * points is vry important !
    *
    * @param x1 x-coord of first point
    * @param y1 y-coord of first point
    * @param x2 x-coord of second point
    * @param y2 y-coord of second point
    */
  private[JSONReaders] case class FlowLine_JSON(x1: Double, y1: Double, x2: Double, y2: Double)

  /**
    * Reads the JSON structure into a [[FlowLine_JSON]] object. No validation on arguments is done.
    */
  implicit val FlowLine_JSONReads: Reads[FlowLine_JSON] = (
    (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double]
    ) (FlowLine_JSON.apply _)

  /**
    * When flow separators are used, the zones must be adapted to make sure that pedestrians head towards the correct
    * side of the separation. This object contains multiple coordinates for each corner. A linear interpolation is
    * done between the lines. Each argument (except the name field) is a vector containing the extremities of the
    * corodinates.
    *
    * @param name          human readable ID of the zone
    * @param x1            bottom left
    * @param y1            bottom left
    * @param x2            bottom right
    * @param y2            bottom right
    * @param x3            top right
    * @param y3            top right
    * @param x4            top left
    * @param y4            top left
    * @param overridenZone name of the zone this zone replaces
    *
    */
  private[JSONReaders] case class VertexOverride_JSON(name: String,
                                                      x1: Vector[Double],
                                                      y1: Vector[Double],
                                                      x2: Vector[Double],
                                                      y2: Vector[Double],
                                                      x3: Vector[Double],
                                                      y3: Vector[Double],
                                                      x4: Vector[Double],
                                                      y4: Vector[Double],
                                                      overridenZone: Option[String])

  /**
    * Reads the JSON structure into a [[VertexOverride_JSON]] object. No validation on arguments is done.
    */
  implicit val VertexOverride_JSON_JSONReads: Reads[VertexOverride_JSON] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "x1").read[Vector[Double]] and
      (JsPath \ "y1").read[Vector[Double]] and
      (JsPath \ "x2").read[Vector[Double]] and
      (JsPath \ "y2").read[Vector[Double]] and
      (JsPath \ "x3").read[Vector[Double]] and
      (JsPath \ "y3").read[Vector[Double]] and
      (JsPath \ "x4").read[Vector[Double]] and
      (JsPath \ "y4").read[Vector[Double]] and
      (JsPath \ "overrides").readNullable[String]
    ) (VertexOverride_JSON.apply _)

  /**
    * A moving barrier which can separate the pedestrian flows. The barrier has two sets of coordinates at each end
    * which correspond to the extremities of each end of the barrier. There are also the sets of flow lines through
    * which the pedestrian flows will be computed. Finally, some zones must be updated to include movable corners,
    * this also implies updating the graph itself.
    *
    * @param x1a        x-coord of first end, first extremity
    * @param y1a        y-coord of first end, first extremity
    * @param x1b        x-coord of first end, second extremity
    * @param y1b        y-coord of first end, second extremity
    * @param x2a        x-coord of second end, first extremity
    * @param y2a        y-coord of second end, first extremity
    * @param x2b        x-coord of second end, second extremity
    * @param y2b        y-coord of second end, second extremity
    * @param inf_1      first end inflow lines
    * @param inf_2      second end inflow lines
    * @param overZone_1 first end zones to update
    * @param overZone_2 second end zones to update
    * @param overConn   connections to override
    */
  private[JSONReaders] case class FlowSeparator_JSON(x1a: Double,
                                                     y1a: Double,
                                                     x1b: Double,
                                                     y1b: Double,
                                                     x2a: Double,
                                                     y2a: Double,
                                                     x2b: Double,
                                                     y2b: Double,
                                                     inf_1: Vector[FlowLine_JSON],
                                                     inf_2: Vector[FlowLine_JSON],
                                                     overZone_1: Vector[VertexOverride_JSON],
                                                     overZone_2: Vector[VertexOverride_JSON],
                                                     overConn: Vector[Connectivity_JSON])

  /**
    * Reads the JSON structure into a [[FlowSeparator_JSON]] object. No validation on arguments is done.
    */
  implicit val FlowSeparator_JSONReads: Reads[FlowSeparator_JSON] = (
    (JsPath \ "x1a").read[Double] and
      (JsPath \ "y1a").read[Double] and
      (JsPath \ "x1b").read[Double] and
      (JsPath \ "y1b").read[Double] and
      (JsPath \ "x2a").read[Double] and
      (JsPath \ "y2a").read[Double] and
      (JsPath \ "x2b").read[Double] and
      (JsPath \ "y2b").read[Double] and
      (JsPath \ "inflow_lines_1").read[Vector[FlowLine_JSON]] and
      (JsPath \ "inflow_lines_2").read[Vector[FlowLine_JSON]] and
      (JsPath \ "overriden_zones_1").read[Vector[VertexOverride_JSON]] and
      (JsPath \ "overriden_zones_2").read[Vector[VertexOverride_JSON]] and
      (JsPath \ "overriden_connections").read[Vector[Connectivity_JSON]]
    ) (FlowSeparator_JSON.apply _)

  private[JSONReaders] case class MonitoredAreas_JSON(name: String, x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double, x4: Double, y4: Double, targetDensity: Double)

  /**
    * Reads a JSON structure into a [[Vertex_JSON]] object. No validation on the arguments is done.
    */
  implicit val MonitoredAreas_JSON_Reads: Reads[MonitoredAreas_JSON] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double] and
      (JsPath \ "x3").read[Double] and
      (JsPath \ "y3").read[Double] and
      (JsPath \ "x4").read[Double] and
      (JsPath \ "y4").read[Double] and
      (JsPath \ "target_density").read[Double]
    ) (MonitoredAreas_JSON.apply _)


  // ******************************************************************************************
  //                   CASE CLASSES AND IMPLICIT CONVERSIONS FOR CONTINUOUS SPACE
  // ******************************************************************************************

  /** Wall class reader for interactions with pedestrians
    *
    * @param x1 x coord of first point
    * @param y1 y coord of first point
    * @param x2 x coord of second point
    * @param y2 y coord of second point
    */
  private[JSONReaders] case class Wall_JSON(comment: String, x1: Double, y1: Double, x2: Double, y2: Double, wallType: Int)

  /**
    * Reads the JSON structure into a [[Wall_JSON]] object. No validation on arguments is done.
    */
  implicit val WallReads: Reads[Wall_JSON] = (
    (JsPath \ "comment").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double] and
      (JsPath \ "type").read[Int]
    ) (Wall_JSON.apply _)


  // ******************************************************************************************
  //         CASE CLASSES AND IMPLICIT CONVERSIONS FOR CONTINUOUS SPACE WITH DOORWAYS
  //         These elements are used for the Guo2011 route choice model.
  // ******************************************************************************************


  /**
    * Definition of a doorway. These are integrated into the walls and influence the potential field.
    *
    * @param comment human readable unique name
    * @param x1      x-coord of first end
    * @param y1      y-coord of first end
    * @param x2      x-coord of second end
    * @param y2      y-coord of second end
    */
  case class Doorway_JSON(comment: String, x1: Double, y1: Double, x2: Double, y2: Double) {
    val startPoint: Position = new Position(x1, y1)
    val endPoint: Position = new Position(x2, y2)
  }

  /**
    * Reads the JSON structure into a [[Doorway_JSON]] object. No validation on arguments is done.
    */
  implicit val DoorwayReads: Reads[Doorway_JSON] = (
    (JsPath \ "comment").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double]
    ) (Doorway_JSON.apply _)

  // ******************************************************************************************
  //                          IMPLICIT CONVERSION TO InfraGraphReader object
  // ******************************************************************************************

  /**
    * Reads the JSON structure into a [[InfraGraphParser]] object. No validation on arguments is done.
    */
  implicit val InfraGraphParserReads: Reads[InfraGraphParser] = (
    (JsPath \ "location").read[String](minLength[String](2)) and
      (JsPath \ "sublocation").read[String](minLength[String](2)) and
      (JsPath \ "nodes").read[Vector[Vertex_JSON]] and
      (JsPath \ "connectivity").read[Vector[Connectivity_JSON]] and
      (JsPath \ "connectivity_level_change").read[Vector[Connectivity_JSON]] and
      (JsPath \ "flow_gates").read[Vector[FlowGates_JSON]] and
      (JsPath \ "controlled_areas").read[Vector[MonitoredAreas_JSON]] and
      (JsPath \ "binary_gates").read[Vector[FlowGates_JSON]] and
      (JsPath \ "moving_walkways").read[Vector[MovingWalkways_JSON]] and
      (JsPath \ "flow_separators").read[Vector[FlowSeparator_JSON]] and
      (JsPath \ "alternate_graphs").read[Vector[ConnectivityAlternatives_JSON]]
    ) (InfraGraphParser.apply _)

  // ******************************************************************************************
  //                          IMPLICIT CONVERSION TO ContinuousSpaceParser object
  // ******************************************************************************************

  /**
    * Reads the JSON structure into a [[ContinuousSpaceParser]] object. No validation on arguments is done.
    */
  implicit val InfraSFParserReads: Reads[ContinuousSpaceParser] = (
    (JsPath \ "location").read[String](minLength[String](2)) and
      (JsPath \ "sublocation").read[String](minLength[String](2)) and
      (JsPath \ "walls").read[Vector[Wall_JSON]]
    ) (ContinuousSpaceParser.apply _)

  // ******************************************************************************************
  //                   IMPLICIT CONVERSION TO ContinuousSpaceParserWithDoors object
  // ******************************************************************************************

  /**
    * Reads the JSON structure into a [[ContinuousSpaceParserWithDoors]] object. No validation on arguments is done.
    */
  implicit val InfraSFParserWithDoorReads: Reads[ContinuousSpaceParserWithDoors] = (
    (JsPath \ "location").read[String](minLength[String](2)) and
      (JsPath \ "sublocation").read[String](minLength[String](2)) and
      (JsPath \ "walls").read[Vector[Wall_JSON]] and
      (JsPath \ "doorways").read[Vector[Doorway_JSON]]
    ) (ContinuousSpaceParserWithDoors.apply _)


  // ******************************************************************************************
  //                   IMPLICIT CONVERSION TO PublicTransportScheduleReader_Reads object
  // ******************************************************************************************

  implicit val PublicTransportScheduleReader_Reads: Reads[PublicTransportScheduleReader] = (
    (JsPath \ "location").read[String](minLength[String](1)) and
      (JsPath \ "trains").read[Vector[Vehicle_JSON]]
    ) (PublicTransportScheduleReader.apply _)
}
