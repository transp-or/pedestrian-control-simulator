package hubmodel.supply

import java.io.{BufferedWriter, File, FileWriter}

import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource

/** Container for the name conversions
  *
  * @param nameMapFile location of the csv file containing the name mappings.
  */
class NameConversions(nameMapFile: String) {

  /** Reads the file passed in the constructor and builds the maps. The final assert checks the sizes if each map.
    * These should match if the all keys and values are unique, which should be the case.
    */
  val string2IntMap: Map[String, Int] = {
    val bufferedSource = scala.io.Source.fromFile(nameMapFile)
    for (line <- bufferedSource.getLines()) yield {
      val cols = line.split(',').map(_.trim)
      cols(0) -> cols(1).toInt
    }
  }.toMap
  val int2StringMap: Map[Int, String] = string2IntMap.map(_.swap)
  assert(string2IntMap.size == int2StringMap.size)
}

/** Reads or writes the infrastructure specification to JSON files
  *
  * @param nameMapFile csv file storing the map. First column is string names and second column is int names.
  */
class ODInfrastructureParser(nameMapFile: String) {

  /** Reads the file passed in the constructor and builds the maps. The final assert checks the sizes if each map.
    * These should match if the all keys and values are unique, which should be the case.
    */
  /*val string2IntMap: Map[String, Int] = {
    val bufferedSource = io.Source.fromFile(nameMapFile)
    for (line <- bufferedSource.getLines()) yield {
      val cols = line.split(',').map(_.trim)
      cols(0) -> cols(1).toInt
    }
  }.toMap
  val int2StringMap: Map[Int, String] = string2IntMap.map(_.swap)
  assert(string2IntMap.size == int2StringMap.size)*/
  val nameMappings = new NameConversions(nameMapFile)

  /** Writes the infrastructure to JSON file using the names map passed as argument.
    * This method writes an OD network
    *
    * @param net      network to write
    * @param fileName file name to write to
    */
  def writeInfraOD(net: InfraODParser, fileName: String): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Json.prettyPrint(Json.toJson(net)))
    bw.close()
  }

  /** Reads the infrastructure from JSON and converts the string names to int using the map.
    * This method reads an OD network. The station is represented as a distance OD matrix.
    *
    * @param file file where the infrastructure specification is located
    * @return an object storing the network
    */
  def readInfraOD(file: String): InfraODModel = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[InfraODParser] match {
      case s: JsSuccess[InfraODParser] => new InfraODModel(s.get, nameMappings)
      case e: JsError => throw new Error("Error while parsing connectionMatrix file: " + JsError.toJson(e).toString())
    }
  }
}

/** Infrastructure modelled as distance OD matrix. The only information which is used to define a hub is the
  * distance between each OD pair. In the JSON file specifying the distance nodes are named using strings which
  * are then converted to ints when being read.
  *
  * The notion of 'node' is associated with a capacity if the physical location corresponds to a doorway
  * or anything with bottleneck. Ramps, stairs or doors are some examples.
  *
  * @param location            general geographical location of the infrastructure
  * @param subLocation         sub specification of the infrastructure
  * @param connectionMatrix    OD matrix stored as a vector of OD pairs
  * @param nodeThroughput      pedestrian capacity limitation of a given node
  * @param nodeToPlatformInput mapping from nodes to platforms. Answers the question on which platform is a given node located
  */
case class InfraODParser(location: String,
                         subLocation: String,
                         connectionMatrix: Vector[ODPairWithoutCapacity],
                         nodeThroughput: Vector[NodeThroughput],
                         nodeToPlatformInput: Vector[Node2PlatMapping],
                         platform2Track: Vector[Platform2TrackMapping]
                        ) extends Infrastructure


/** Infrastructure modelled as distance OD matrix. The only information which is used to define a hub is the
  * distance between each OD pair. In the JSON file specifying the distance nodes are named using strings which
  * are then converted to ints when being read.
  *
  * The notion of 'node' is associated with a capacity if the physical location corresponds to a doorway
  * or anything with bottleneck. Ramps, stairs or doors are some examples.
  *
  * This class processes the raw data which is read from the JSON file. This data is passed in the infraRaw argument.
  * It is a result from the readInfrOD method of
  *
  * @param infraRaw     Raw data read from the JSON file
  * @param nameMappings map of string names to int names and reverse
  */
class InfraODModel(infraRaw: InfraODParser, val nameMappings: NameConversions) extends Infrastructure {
  val location: String = infraRaw.location
  val subLocation: String = infraRaw.subLocation
  val network: Map[Tuple2[NodeIDOld, NodeIDOld], Double] = {
    val tmpNetwork: Map[Tuple2[NodeIDOld, NodeIDOld], Double] = infraRaw.connectionMatrix.map(od => (od.O, od.D) -> od.distance).toMap
    val complementOfTmpNetwork: Map[Tuple2[NodeIDOld, NodeIDOld], Double] = for (pairs <- tmpNetwork if !tmpNetwork.keySet.contains(pairs._1.swap)) yield {
      pairs._1.swap -> pairs._2
    }
    tmpNetwork ++ complementOfTmpNetwork
  }

  val nodes: Vector[NodeIDOld] = infraRaw.connectionMatrix.flatMap(od => List(od.O, od.D)).distinct
  val nodeCapacity: Map[NodeIDOld, Double] = infraRaw.nodeThroughput.map(v => v.ID -> v.throughput).toMap
  val nodesWithCapacity: Vector[NodeIDOld] = nodeCapacity.keys.toVector
  val isCapacitated: NodeIDOld => Boolean = node => this.nodesWithCapacity.contains(node)
  val node2Platform: Map[NodeIDOld, TrackIDOld] = infraRaw.nodeToPlatformInput.map(x => x.node -> nameMappings.string2IntMap(x.plat)).toMap
  val platform2Node: Map[TrackIDOld, Iterable[NodeIDOld]] = node2Platform.groupBy(_._2).mapValues(_.keys)
  val track2platform: Map[Int, Int] = infraRaw.platform2Track.flatMap(p => p.tracks.map(t => t -> nameMappings.string2IntMap(p.platform))).toMap

  def isOnSamePlatform(trackA: Int, trackB: Int): Boolean = track2platform(trackA) == track2platform(trackB)


  def flowRate(flowCritic: Double)(flowActual: Double): Double = {
    if (flowActual > flowCritic) flowCritic
    //else flowActual
    else flowCritic
  }

  val test: NodeIDOld => Double = node => nodeCapacity(node)
}
