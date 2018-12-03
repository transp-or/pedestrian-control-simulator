package hubmodel {

  /**
    * Created by nicholas on 3/6/17.
    */

  import play.api.libs.functional.syntax._
  import play.api.libs.json.Reads.{min, minLength}
  import play.api.libs.json._

  /** This package contains the classes and methods which are needed for building
    *
    */
  package object supply {

    abstract class NodeParent(val ID: String) {
      def canEqual(a: Any): Boolean = a.isInstanceOf[NodeParent]

      override def equals(that: Any): Boolean =
        that match {
          case that: NodeParent => that.canEqual(this) && this.hashCode == that.hashCode
          case _ => false
        }

      override def hashCode: Int = {
        this.ID.hashCode
      }
    }

    class NodeID_New(ID: String, val humanID: String) extends NodeParent(ID) {
      def this(ID: String) = this(ID, ID)

      override def toString: ODIDOld = this.ID

      override def canEqual(a: Any): Boolean = a.isInstanceOf[NodeID_New]

      override def equals(that: Any): Boolean =
        that match {
          case that: NodeID_New => that.canEqual(this) && this.hashCode == that.hashCode
          case _ => false
        }

      override def hashCode: Int = {
        this.ID.hashCode
      }
    }

    object NodeID_New {
      def apply(id: String, humandID: String): NodeID_New = new NodeID_New(id, humandID)
    }

    class StopID_New(ID: String, humanID: String) extends NodeParent(ID.toString) {
      def this(ID: String) = this(ID, ID.toString)

      override def toString: ODIDOld = this.ID

      override def canEqual(a: Any): Boolean = a.isInstanceOf[StopID_New]

      override def equals(that: Any): Boolean =
        that match {
          case that: StopID_New => that.canEqual(this) && this.hashCode == that.hashCode
          case _ => false
        }

      override def hashCode: Int = {
        this.ID.hashCode
      }
    }

    object StopID_New {
      def apply(id: String, humandID: String): StopID_New = new StopID_New(id, humandID)
    }

    class TrainID_New(ID: String, humanID: String) extends NodeParent(ID) {
      def this(ID: String) = this(ID, ID)

      override def toString: ODIDOld = this.ID

      override def canEqual(a: Any): Boolean = a.isInstanceOf[TrainID_New]

      override def equals(that: Any): Boolean =
        that match {
          case that: TrainID_New => that.canEqual(this) && this.hashCode == that.hashCode
          case _ => false
        }

      override def hashCode: Int = {
        this.ID.hashCode
      }
    }

    object TrainID_New {
      def apply(id: String, humandID: String): TrainID_New = new TrainID_New(id, humandID)
    }

    type NodeIDOld = String
    type TrackIDOld = Int
    type TrainIDOld = String
    type ODIDOld = String


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

