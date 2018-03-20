package hubmodel.demand

import java.time.LocalTime

import hubmodel.NewTime
import hubmodel.supply.{NodeID, NodeID_New, NodeParent, ODID, TrackID, TrackID_New, TrainID, TrainID_New}
//import pedtrack.StringImprovements
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource
import scala.util.Try

/* ----------------------------------------------------------------------------------
                                  TRACK TO NODE MAPPING
-----------------------------------------------------------------------------------*/

/** Pairs of tracks and corresponding ZONES OR NODES ?
  *
  * @param track id of track, as [[TrackID]]
  * @param nodes Vector of [[NodeID]]
  */
case class Track2Nodes(track: Int, nodes: Vector[NodeID])

/** Container for the track to nodes mapping
  *
  * @param loc                    location of this map
  * @param Track2NodeMappingInput raw data
  */
case class Track2NodeMapping(loc: String, private val Track2NodeMappingInput: Vector[Track2Nodes]) {
  val track2Nodes: Map[Int, Vector[NodeID]] = Track2NodeMappingInput.map(t => t.track -> t.nodes).toMap
}

/* ----------------------------------------------------------------------------------
                                  TRAIN TIMETABLE
-----------------------------------------------------------------------------------*/

/** Train object. CAPACITY SHOULD BE CHANGED FOR A TRAIN TYPE AT SOME POINT
  *
  * @param ID       unique identifier (String)
  * @param track    track on which the train arrives
  * @param arr      arrival time
  * @param dep      departure time
  * @param capacity max capacity of the train
  */
case class Train(ID: String, trainType: String, track: Int, arr: Option[LocalTime], dep: Option[LocalTime], capacity: Int) {
  val IDNew: TrainID_New = TrainID_New(ID, ID)
  override def toString: ODID = {
    arr match {
      case Some(str) => {
        dep match {
          case Some(str2) => ID + ", track=" + track + ", arrival @ " + str + ", departure @ " + str2 + " with capacity=" + capacity
          case None => ID + ", track=" + track + ", arrival @ " + str + ", no departure " + " with capacity=" + capacity
        }
      }
      case None => {
        dep match {
          case Some(str2) => ID + ", track=" + track + ", no arrival, " + "departure @ " + str2 + " with capacity=" + capacity
          case None => ID + ", track=" + track + ", no arrival, no departure " + " with capacity=" + capacity
        }
      }
    }

  }
}


/** Storage of the train time table
  *
  * @param loc             in which station does this time table belong to
  * @param _timeTableInput raw content of the time table
  */
case class TrainTimeTable(loc: String, private val _timeTableInput: Vector[Train]) {
  val train2Track: Map[String, TrackID] = _timeTableInput.map(t => t.ID -> t.track).toMap
  val timeTable: Map[String, Train] = _timeTableInput.map(t => t.ID -> t).toMap
}

/** Container for the time table. The time table is stored alongside a map from trains to track.
  *
  * @param file location of the time table json
  */
class TimeTable(file: String) {
  private val _timeTable: TrainTimeTable = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[TrainTimeTable] match {
      case s: JsSuccess[TrainTimeTable] => s.get
      case e: JsError => throw new Error("Error while parsing train timetable: " + JsError.toJson(e).toString())
    }
  }

  // time table: a list of trains identified by their ID
  val trains: Map[String, Train] = _timeTable.timeTable

  // map from the train's ID to the track
  val train2TrackMap: Map[String, TrackID] = _timeTable.train2Track
  val train2TrackMapNew: TrainID_New => TrackID_New = trainID => new TrackID_New(_timeTable.train2Track(trainID.ID))


  private val _track2Node: Track2NodeMapping = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Track2NodeMapping] match {
      case s: JsSuccess[Track2NodeMapping] => s.get
      case e: JsError => throw new Error("Error while parsing track to nodes mapping: " + JsError.toJson(e).toString())
    }
  }

  // mapping from tracks to nodes THIS WILL NEED TO BE EXTENDED LATER ON
  val track2Nodes: Map[TrackID, Vector[NodeID]] = _track2Node.track2Nodes

  // mapping from train to nodes
  val train2Nodes: TrainID => Option[Vector[NodeID]] = train => Try {
    track2Nodes(train2TrackMap(train))
  }.toOption


  // mapping from tracks to nodes THIS WILL NEED TO BE EXTENDED LATER ON
  val track2NodesNew: TrackID_New => Vector[NodeID_New] = track => _track2Node.track2Nodes(track.ID).map(n => NodeID_New(n, n.toString))

  // mapping from train to nodes
  val train2NodesNew: TrainID_New => Vector[NodeID_New] = train => track2NodesNew(train2TrackMapNew(train))


  // check if vectors of nodeID are the same
  //@deprecated
  def isOnSamePlatform(t1: TrainID, t2: TrainID): Boolean = {
    train2Nodes(t1) match {
      case Some(x) => train2Nodes(t2) match {
        case Some(y) => x.sorted.zip(y.sorted).forall(p => p._1 == p._2)
        case None => false
      }
      case None => false
    }
    //train2Nodes(t1).get.sorted.zip(train2Nodes(t2).get.sorted).forall(p => p._1==p._2)
  }

  // check if vectors of nodeID are the same
  def isOnSamePlatform(t1: TrainID_New, t2: NodeParent): Boolean = {
    t2 match {
      case tID: TrainID_New => {
        train2Nodes(t1.ID) match {
          case Some(x) => train2Nodes(tID.ID) match {
            case Some(y) => x.sorted.zip(y.sorted).forall(p => p._1 == p._2)
            case None => false
          }
          case None => false
        }
        //train2Nodes(t1).get.sorted.zip(train2Nodes(t2).get.sorted).forall(p => p._1==p._2)
      }
      case _ => false
    }
  }
}


/* ----------------------------------------------------------------------------------
                                  PASSENGER FLOWS
-----------------------------------------------------------------------------------*/

/** Flow of pedestrians
  *
  * @param start time of the beginning of the flows
  * @param end   time of the end of the flow
  * @param O     origin [[NodeID]] of the flow
  * @param D     destination [[NodeID]]
  * @param f     number of people
  */
case class PedestrianFlow(O: String, D: String, private val _start: LocalTime, private val _end: LocalTime, f: Double){
  val start: NewTime = new NewTime(_start.toSecondOfDay)
  val end: NewTime = new NewTime(_end.toSecondOfDay)
}

case class PTFlow(private val _origin: String, private val _destination: String, f: Double){
  val origin: TrainID_New = if (_origin.charAt(0) == 'T') new TrainID_New(_origin.substring(2))
  else throw new IllegalArgumentException("Data in time table file is badly formatted: " + _origin.charAt(0) + " while T is expected. (" + _origin + ")")

  val destination: NodeParent = if (_destination.charAt(0) == 'T') new TrainID_New(_destination.substring(2))
  else if (_destination.charAt(0) == 'S') new NodeID_New(_destination.substring(2))
  else throw new IllegalArgumentException("Data in time table file is badly formatted: " + _destination.charAt(0) + " while S is expected. (" + _destination + ")")
}

abstract class PedestrianFlow_New_Parent(val O: NodeParent, val D: NodeParent, val f: Double) {
  override def toString: NodeID = "(" + O + ", " + D + ", " + f + ")"
}

case class PedestrianFlow_New(override val O: NodeID_New, override val D: NodeParent, start: NewTime, end: NewTime, override val f: Double) extends PedestrianFlow_New_Parent(O, D, f) {
  override def toString: NodeID = "(" + O + ", " + D + ", " + f + ", " + this.start + ", " + this.end + ")"
}

case class PedestrianFlowPT_New(override val O: TrainID_New, override val D: NodeParent, override val f: Double) extends PedestrianFlow_New_Parent(O, D, f)



class PedestrianFlows(file: String,
                      timeTable: TimeTable,
                      useFlows: Boolean) {
  private val _pedestrianFlowData: ODFlowData = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[ODFlowData] match {
      case s: JsSuccess[ODFlowData] => s.get
      case e: JsError => throw new Error("Error while parsing OF flow file: " + JsError.toJson(e).toString())
    }
  }

  val flows: Iterable[PedestrianFlow_New] = if (useFlows) {
    _pedestrianFlowData.flows.map(f => PedestrianFlow_New(NodeID_New(f.O, f.O.toString), NodeID_New(f.D, f.D.toString), f.start, f.end, f.f))
  } else {
    Iterable()
  }

  val flowsPTInduced: Iterable[PedestrianFlowPT_New] = if (useFlows) {
    _pedestrianFlowData.PTflows.map(f => {
      PedestrianFlowPT_New(f.origin, f.destination, f.f)
    })
  } else {
    Iterable()
  }
}


//case class PassengerFlow(O: String, D: String, flow: Double)
case class ODFlowData(loc: String, PTflows: Vector[PTFlow], flows: Vector[PedestrianFlow])
