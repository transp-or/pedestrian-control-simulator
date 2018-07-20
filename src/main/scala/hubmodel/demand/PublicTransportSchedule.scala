package hubmodel.demand

import hubmodel.demand.transit.Vehicle
import hubmodel.supply.{StopID_New, TrainID_New}

class PublicTransportSchedule(loc: String, _schedule: Vector[Vehicle]) {
  val vehicle2Stop: Map[TrainID_New, StopID_New] = _schedule.map (t => t.ID -> t.stop).toMap
  val timeTable: Map[TrainID_New, Vehicle] = _schedule.map (t => t.ID -> t).toMap
  // map from the train's ID to the stop
  //val train2TrackMapNew: TrainID_New => TrackID_New = trainID => new TrackID_New(train2Track(trainID.ID), train2Track(trainID.ID).toString)


  /*private val _track2Node: Track2NodeMapping = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Track2NodeMapping] match {
      case s: JsSuccess[Track2NodeMapping] => s.get
      case e: JsError => throw new Error("Error while parsing track to nodes mapping: " + JsError.toJson(e).toString())
    }
  }*/

  // mapping from tracks to nodes THIS WILL NEED TO BE EXTENDED LATER ON
  //val track2Nodes: Map[TrackIDOld, Vector[NodeIDOld]] = _track2Node.track2Nodes

  // mapping from train to nodes
  /*val train2Nodes: TrainIDOld => Option[Vector[NodeIDOld]] = train => Try {
    track2Nodes(train2TrackMap(train))
  }.toOption*/


  // mapping from tracks to nodes THIS WILL NEED TO BE EXTENDED LATER ON
  //val track2NodesNew: TrackID_New => Vector[NodeID_New] = track => _track2Node.track2Nodes(track.ID).map(n => NodeID_New(n, n.toString))

  // mapping from train to nodes
  //val train2NodesNew: TrainID_New => Vector[NodeID_New] = train => track2NodesNew(train2TrackMapNew(train))


  // check if vectors of nodeID are the same
  //@deprecated
  /*def isOnSamePlatform(t1: TrainIDOld, t2: TrainIDOld): Boolean = {
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
  }*/
}
