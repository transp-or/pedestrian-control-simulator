import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}

import breeze.linalg.DenseVector
import myscala.math.stats.Quantiles
import myscala.output.MapSeqExtensions._
import myscala.output.SeqExtension.SeqWriter
import play.api.libs.functional.syntax._
import play.api.libs.json._
import trackingdataanalysis.pedtrack.{DataSpecification, FunctionsOnPedestrianMaps, PedestrianTrait, Time}

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode


/** Elements common for processing both empirical or simulated data. These include trait, classes and functions.
  *
  */
package trackingdataanalysis {

  package object pedtrack {

    sealed class Vector2D(val X: Double, val Y: Double) {

      def +(that: Vector2D): Vector2D = {
        new Vector2D(this.X + that.X, this.Y + that.Y)
      }

      def -(that: Vector2D): Vector2D = {
        new Vector2D(this.X - that.X, this.Y - that.Y)
      }

      def +(i: Double): Vector2D = {
        new Vector2D(this.X + i, this.Y + i)
      }

      def -(i: Double): Vector2D = {
        new Vector2D(this.X - i, this.Y - i)
      }

      def *(i: Double): Vector2D = {
        new Vector2D(this.X * i, this.Y * i)
      }

      def /(i: Double): Vector2D = {
        new Vector2D(this.X / i, this.Y / i)
      }

      override def toString: String = "(" + X + "," + Y + ")"
    }

    final class NewBetterPosition2D(x: Double, y: Double) extends Vector2D(x, y)

    final class NewBetterDirection2D(x: Double, y: Double) extends Vector2D(x, y)

    final class NewBetterVelocity2D(x: Double, y: Double) extends Vector2D(x, y)

    final class NewBetterAcceleration2D(x: Double, y: Double) extends Vector2D(x, y)

    final class NewBetterForce2D(x: Double, y: Double) extends Vector2D(x, y)


    def norm[T <: Vector2D](a: T, b: T): Double = scala.math.pow((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y), 0.5)

    type NewPosition2D = (Double, Double)

    def norm(a: NewPosition2D, b: NewPosition2D): Double = scala.math.pow((b._1 - a._1) * (b._1 - a._1) + (b._2 - a._2) * (b._2 - a._2), 0.5)


    type NodeID = Int

    type Position = DenseVector[Double]
    type Velocity = DenseVector[Double]

    /** For converting LocalDateTime object to seconds from Epoch */
    val zoneId: ZoneId = ZoneId.systemDefault(); // or: ZoneId.of("Europe/Oslo");


    /** converts a string to java.LocalDateTime with format "yyyy-mm-dd hh:mm:ss"
      *
      * @return LocaDateTime object built from String
      */
    def string2LocalDateTime: String => LocalDateTime = str => LocalDateTime.parse(str, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))


    /** function to convert VisioSafe time to seconds of one day
      *
      * @param hmsms array containing the h, m, s, ms in 3,4,5,6 positions.
      * @return number of seconds passed on that day
      */
    def time2Seconds(hmsms: Array[Int]): Double = hmsms(3) * 3600.0 + hmsms(4) * 60.0 + hmsms(5) + hmsms(6) / 1000.0

    type PedestrianSeq = Seq[PedestrianTrait]
    type PedestrianMap = collection.immutable.Map[Int, PedestrianTrait]
    type MultiDayPedestrianMap = Map[DataSpecification, PedestrianMap]


    /** Aggregates a member from a [[trackingdataanalysis.pedtrack.PedestrianMap]] for each pedestrian into a vector by OD
      *
      * @param pedMap   pedestrian map containing the pedestrians
      * @param quantity string naming which quantity to aggregate
      * @return map where the keys are OD pairs and the values are vectors of the corresponding data
      */
    def aggregateQuantityByOD(pedMap: PedestrianMap,
                              quantity: String
                             ): Map[(Int, Int), Vector[Double]] = {

      /** Recursive helper function which performs the aggregation
        *
        * @param pedMap remaining data to process
        * @param acc    accumulator
        * @return call to itself until all data has been processed
        */
      def aggregateQuantityByODHelper(pedMap: PedestrianMap, acc: Map[(Int, Int), Vector[Double]]): Map[(Int, Int), Vector[Double]] = {
        if (pedMap.isEmpty) acc
        else {
          quantity match {
            case "travelDistance" => aggregateQuantityByODHelper(pedMap.tail, acc + (pedMap.head._2.getODTuple -> (acc.getOrElse(pedMap.head._2.getODTuple, Vector()) :+ pedMap.head._2.travelDistance)))
            case "travelTime" => aggregateQuantityByODHelper(pedMap.tail, acc + (pedMap.head._2.getODTuple -> (acc.getOrElse(pedMap.head._2.getODTuple, Vector()) :+ pedMap.head._2.travelTime)))
            case "freeFlowVel" => aggregateQuantityByODHelper(pedMap.tail, acc + (pedMap.head._2.getODTuple -> (acc.getOrElse(pedMap.head._2.getODTuple, Vector()) :+ pedMap.head._2.meanVelocity)))
          }
        }
      }

      aggregateQuantityByODHelper(pedMap, Map())
    }

    /** Aggregates a member belonging to [[trackingdataanalysis.pedtrack.PedestrianTrait]] which are stored in a vector into a map indexed by OD pairs
      *
      * @param pedSeq   all pedestrians are stored in this container
      * @param quantity string naming which quantity to aggregate
      * @return map where the keys are OD pairs and the values are vectors of the corresponding data
      */
    def aggregateQuantityByOD(pedSeq: Seq[PedestrianTrait],
                              quantity: String
                             ): Map[(Int, Int), Vector[Double]] = {

      /** Recursive helper function which performs the aggregation
        *
        * @param pedSeq remaining data to process
        * @param acc    accumulator
        * @return call to itself until all data has been processed
        */
      def aggregateQuantityByODHelper(pedSeq: Seq[PedestrianTrait], acc: Map[(NodeID, NodeID), Vector[Double]]): Map[(Int, Int), Vector[Double]] = {
        if (pedSeq.isEmpty) acc
        else {
          quantity match {
            case "travelDistance" => aggregateQuantityByODHelper(pedSeq.tail, acc + (pedSeq.head.getODTuple -> (acc.getOrElse(pedSeq.head.getODTuple, Vector()) :+ pedSeq.head.travelDistance)))
            case "travelTime" => aggregateQuantityByODHelper(pedSeq.tail, acc + (pedSeq.head.getODTuple -> (acc.getOrElse(pedSeq.head.getODTuple, Vector()) :+ pedSeq.head.travelTime)))
            case "freeFlowVel" => aggregateQuantityByODHelper(pedSeq.tail, acc + (pedSeq.head.getODTuple -> (acc.getOrElse(pedSeq.head.getODTuple, Vector()) :+ pedSeq.head.meanVelocity)))
          }
        }
      }

      aggregateQuantityByODHelper(pedSeq, Map())
    }

    /** Computes the quantiles for each vecto r stored in the Map
      *
      * @param data         for each OD pair, a vector storing data
      * @param quantilesFun Quantile function
      * @return For each OD pair, the quantiles of the data
      */
    def getQuantiles(data: Map[(Int, Int), Vector[Double]], quantilesFun: Seq[Double] => Quantiles[Double]): Map[(Int, Int), Quantiles[Double]] = data.mapValues(quantilesFun)


    /** Methods for filtering vectors of pedestrians
      *
      * @param x storage of the pedestrian objects
      */
    implicit class PedestrianSeqFunctions(x: PedestrianSeq) extends FunctionsOnPedestrianMaps {

      /** Filter by time interval of a given day
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByTime(start: Long = 0, end: Long = 86400): PedestrianSeq = {
        x.filter(p => p.entryTime > start && p.exitTime < end)
      }

      /** Returns pedestrians which entered and exited at specific nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByOD(O: Int, D: Int): PedestrianSeq = {
        x.filter(p => p.oZone == O && p.dZone == D)
      }

      /** Returns pedestrians which entered or exited in given lists of nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByOD(O: List[Int], D: List[Int]): PedestrianSeq = {
        x.filter(p => O.contains(p.oZone) && D.contains(p.dZone))
      }

      /** Returns the pedestrians which entered in O and exit in a list of exit nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByOD(O: Int, D: List[Int]): PedestrianSeq = {
        x.filter(p => p.oZone == O && D.contains(p.dZone))
      }

      /** Returns the pedestrians which entered via the list O and exited at D
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByOD(O: List[Int], D: Int): PedestrianSeq = {
        x.filter(p => O.contains(p.oZone) && p.dZone == D)
      }

      /** Returns the pedestrians entering via a single node
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByOrigin(O: Int): PedestrianSeq = {
        x.filter(p => p.oZone == O)
      }

      /** Returns the pedetrians who entered in a list of node
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByOrigin(O: List[Int]): PedestrianSeq = {
        x.filter(p => O.contains(p.oZone))
      }

      /** Returns the pedestrians who exited via a list of nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByDestination(D: Int): PedestrianSeq = {
        x.filter(p => p.dZone == D)
      }

      /** Returns the pedestrians who exited via a list of nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByDestination(D: List[Int]): PedestrianSeq = {
        x.filter(p => D.contains(p.dZone))
      }

      /** Returns pedestrians who accomplished specfiic OD pairs
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianSeq]] with the filtered data
        */
      def filterByODPairs(OD: List[(Int, Int)]): PedestrianSeq = {
        x.filter(p => OD.contains((p.oZone, p.dZone)))
      }

      def mapQuantityToVector(data: String): Vector[Double] = {
        data match {
          case "travelDistance" => x.map(p => p.travelDistance).toVector
          case "travelTime" => x.map(p => p.travelTime).toVector
          case "meanVelocity" => x.map(p => p.meanVelocity).toVector
          case other => throw new IllegalArgumentException("choice is not valid")
        }
      }

      /** Prints the data to a CSV file after having specified which quantity to print
        *
        * @param data     String specifying the quantity to print
        * @param filename name of the file
        * @param path     path where to write the file with trailing / defautl is empty ""
        */
      def writeQuantityToCSV(data: String, filename: String, path: String = ""): Unit = {
        x.mapQuantityToVector(data).writeToCSV(filename, path)
      }

      /** Prints data to JSON file. The JSON key is the quantity which is passed as argument
        *
        * @param data     String specifying the quantity to print
        * @param filename name of the file
        * @param path     path where to write the file with trailing / defautl is empty ""
        */
      def writeQuantityToJSON(data: String, filename: String, path: String = ""): Unit = {
        x.mapQuantityToVector(data).writeToJSON(data, filename, path)
      }
    }


    /** Filtering methods on [[trackingdataanalysis.pedtrack.PedestrianMap]]
      *
      * @param x implicit map where the key is the pedestrian ID
      */
    implicit class PedestrianMapFunctions(x: PedestrianMap) extends FunctionsOnPedestrianMaps {

      /** Filter by time interval of a given day
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByTime(start: Long = 0, end: Long = 86400): PedestrianMap = {
        x.filter(p => p._2.entryTime > start && p._2.exitTime < end)
      }

      /** Returns pedestrians which entered and exited at specific nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByOD(O: Int, D: Int): PedestrianMap = {
        x.filter(p => p._2.oZone == O && p._2.dZone == D)
      }

      /** Returns pedestrians which entered or exited in given lists of nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByOD(O: List[Int], D: List[Int]): PedestrianMap = {
        x.filter(p => O.contains(p._2.oZone) && D.contains(p._2.dZone))
      }

      /** Returns the pedestrians which entered in O and exit in a list of exit nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByOD(O: Int, D: List[Int]): PedestrianMap = {
        x.filter(p => p._2.oZone == O && D.contains(p._2.dZone))
      }

      /** Returns the pedestrians which entered via the list O and exited at D
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByOD(O: List[Int], D: Int): PedestrianMap = {
        x.filter(p => O.contains(p._2.oZone) && p._2.dZone == D)
      }

      /** Returns the pedestrians entering via a single node
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByOrigin(O: Int): PedestrianMap = {
        x.filter(p => p._2.oZone == O)
      }

      /** Returns the pedetrians who entered in a list of node
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByOrigin(O: List[Int]): PedestrianMap = {
        x.filter(p => O.contains(p._2.oZone))
      }

      /** Returns the pedestrians who exited via a list of nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByDestination(D: Int): PedestrianMap = {
        x.filter(p => p._2.dZone == D)
      }

      /** Returns the pedestrians who exited via a list of nodes
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByDestination(D: List[Int]): PedestrianMap = {
        x.filter(p => D.contains(p._2.dZone))
      }

      /** Returns pedestrians who accomplished specfiic OD pairs
        *
        * @return [[trackingdataanalysis.pedtrack.PedestrianMap]] with the filtered data
        */
      def filterByODPairs(OD: List[(Int, Int)]): PedestrianMap = {
        x.filter(p => OD.contains((p._2.oZone, p._2.dZone)))
      }

      def mapQuantityToVector(data: String): Vector[Double] = {
        data match {
          case "travelDistance" => x.map(p => p._2.travelDistance).toVector
          case "travelTime" => x.map(p => p._2.travelTime).toVector
          case "meanVelocity" => x.map(p => p._2.meanVelocity).toVector
          case other => throw new IllegalArgumentException("choice is not valid")
        }
      }

      /** Prints the data to a CSV file after having specified which quantity to print
        *
        * @param data     String specifying the quantity to print
        * @param filename name of the file
        * @param path     path where to write the file with trailing / defautl is empty ""
        */
      def writeQuantityToCSV(data: String, filename: String, path: String = ""): Unit = {
        x.mapQuantityToVector(data).writeToCSV(filename, path)
      }

      /** Prints data to JSON file. The JSON key is the quantity which is passed as argument
        *
        * @param data     String specifying the quantity to print
        * @param filename name of the file
        * @param path     path where to write the file with trailing / defautl is empty ""
        */
      def writeQuantityToJSON(data: String, filename: String, path: String = ""): Unit = {
        x.mapQuantityToVector(data).writeToJSON(data, filename, path)
      }
    }

    /** Filtering on the results of a [[trackingdataanalysis.pedtrack.visiosafe.MultiDayAggregateProcessor]] for example.
      * The key of the Map is a [[DataSpecification]] object and the value is a [[PedestrianMap]]
      *
      * @param x input data to filter
      */
    implicit class MultiDayPedestrianMapFunctions(x: MultiDayPedestrianMap) extends FunctionsOnPedestrianMaps {

      /** Filter by time interval of a given day
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByTime(start: Long = 0, end: Long = 86400): MultiDayPedestrianMap = {
        x.mapValues(pedMap => pedMap.filter(p => p._2.entryTime > start && p._2.exitTime < end))
      }

      /** Returns pedestrians which entered and exited at specific nodes
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByOD(O: Int, D: Int): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => p._2.oZone == O && p._2.dZone == D))
      }

      /** Returns pedestrians which entered or exited in given lists of nodes
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByOD(O: List[Int], D: List[Int]): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => O.contains(p._2.oZone) && D.contains(p._2.dZone)))
      }

      /** Returns the pedestrians which entered in O and exit in a list of exit nodes
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByOD(O: Int, D: List[Int]): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => p._2.oZone == O && D.contains(p._2.dZone)))
      }

      /** Returns the pedestrians which entered via the list O and exited at D
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByOD(O: List[Int], D: Int): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => O.contains(p._2.oZone) && p._2.dZone == D))
      }

      /** Returns the pedestrians entering via a single node
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByOrigin(O: Int): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => p._2.oZone == O))
      }

      /** Returns the pedetrians who entered in a list of node
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByOrigin(O: List[Int]): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => O.contains(p._2.oZone)))
      }

      /** Returns the pedestrians who exited via a list of nodes
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByDestination(D: Int): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => p._2.dZone == D))
      }

      /** Returns the pedestrians who exited via a list of nodes
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByDestination(D: List[Int]): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => D.contains(p._2.dZone)))
      }

      /** Returns pedestrians who accomplished specfiic OD pairs
        *
        * @return filtered data as [[MultiDayPedestrianMap]]
        */
      def filterByODPairs(OD: List[(Int, Int)]): Map[DataSpecification, PedestrianMap] = {
        x.mapValues(pedMap => pedMap.filter(p => OD.contains((p._2.oZone, p._2.dZone))))
      }

      def mapQuantityToVector(data: String): Vector[Double] = {
        data match {
          case "travelDistance" => x.flatMap(pedMap => pedMap._2.map(p => p._2.travelDistance).toVector).toVector
          case "travelTime" => x.flatMap(pedMap => pedMap._2.map(p => p._2.travelTime).toVector).toVector
          case "meanVelocity" => x.flatMap(pedMap => pedMap._2.map(p => p._2.meanVelocity).toVector).toVector
          case other => throw new IllegalArgumentException("choice is not valid")
        }
      }

      /** Returns a one vector per original item as a map where the key is the [[trackingdataanalysis.pedtrack.DataSpecification]] object
        *
        * @param data String specifying which variable to extract
        * @return one vector per (key, value) pair with the data extracted from the pedestrians
        */
      def mapToVectorByDay(data: String): Map[DataSpecification, Vector[Double]] = {
        data match {
          case "travelDistance" => x.mapValues(pedMap => pedMap.map(p => p._2.travelDistance).toVector)
          case "travelTime" => x.mapValues(pedMap => pedMap.map(p => p._2.travelTime).toVector)
          case "meanVelocity" => x.mapValues(pedMap => pedMap.map(p => p._2.meanVelocity).toVector)
          case other => throw new IllegalArgumentException("choice is not valid")
        }
      }

      /** Prints a given variable to a CSV file
        *
        * @param data     String naming the variable to print
        * @param filename name of the file
        * @param path     path where to write the file with trailing / defautl is empty ""
        */
      def writeQuantityToCSV(data: String, filename: String, path: String = ""): Unit = {
        x.mapToVectorByDay(data).writeToCSV(filename, path)
      }

      /** Prints a given variable of each pedestrian to a JSON file. The keys are taken from the keys of the map.
        *
        * @param data     String naming the variable to print
        * @param filename name of the file
        * @param path     path where to write the file with trailing / defautl is empty ""
        */
      def writeQuantityToJSON(data: String, filename: String, path: String = ""): Unit = {
        x.mapToVectorByDay(data).writeToJSON(data, filename, path)
      }
    }

    val decimalWrites = new Writes[BigDecimal] {
      def writes(o: BigDecimal): JsValue = JsString(o.setScale(2, RoundingMode.HALF_UP).toString())
    }

    case class Pedestrian_JSON(ID: String,
                               time: Iterable[Double],
                               x: ArrayBuffer[Double],
                               y: ArrayBuffer[Double],
                               vx: Iterable[Double],
                               vy: Iterable[Double])

    implicit val Pedestrian_JSONWrites: Writes[Pedestrian_JSON] = (
      (JsPath \ "id").write[String] and
        (JsPath \ "time").write[Iterable[Double]] and
        (JsPath \ "x_pos").write[ArrayBuffer[Double]] and
        (JsPath \ "y_pos").write[ArrayBuffer[Double]] and
        (JsPath \ "x_vel").write[Iterable[Double]] and
        (JsPath \ "y_vel").write[Iterable[Double]]
      ) (unlift(Pedestrian_JSON.unapply))


    /** Extensions to String.
      *
      * @param s the string on which to call the method.
      */
    implicit class StringImprovements(s: String) {

      /** Converts "YYYY-MM-DD hh:mm:ss:ms" to a [[Time]] object
        *
        * @return [[hubmodel.Time]] object created from the string
        */
      def toTime: Time = s.count(_ == ':') match {
        case 3 => new Time(s.split(Array('-', '-', ' ', ':', ':', ':')).map(c => c.toInt))
        case 2 => new Time(s.split(Array('-', '-', ' ', ':', ':')).map(c => c.toInt) ++ Array(0))
      }
    }


  }

}
