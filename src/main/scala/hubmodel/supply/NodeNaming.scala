package hubmodel.supply

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.io.BufferedSource

/** Container for storing the maps of node names from ints to strings ance vice versa
  *
  * @param file CSV file containing the map. First column is String name and second column is Int name
  */
class NodeNaming(file: String) {

  case class String2Int(string: String, int: Int)

  implicit val String2IntReads: Reads[String2Int] = (
    (JsPath \ "string").read[String](minLength[String](1)) and
      (JsPath \ "int").read[Int]
    ) (String2Int.apply _)

  //case class String2IntVector(vec: Vector[String2Int])
  //implicit val String2IntVectorReads: Reads[String2IntVector] = ((JsPath \ "strings2ints").read[Vector[String2Int]])(String2IntVector.apply _)


  private val _String2Int: Vector[String2Int] = {
    val source: BufferedSource = scala.io.Source.fromFile(file)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Vector[String2Int]] match {
      case s: JsSuccess[Vector[String2Int]] => s.get
      case e: JsError => throw new Error("Error while parsing node naming file: " + JsError.toJson(e).toString())
    }
  }


  /*val string2IntMap: Map[String, Int] = {
    val bufferedSource = scala.io.Source.fromFile(file)
    for (line <- bufferedSource.getLines()) yield {
      val cols = line.split(',').map(_.trim)
      cols(0) -> cols(1).toInt
    }
  }.toMap*/

  //val string2IntMap: Map[String, NodeID] = _String2Int.map(p => p.string -> p.int).toMap
  //println(string2IntMap)
  //val int2StringMap: Map[NodeID, String] = string2IntMap.map(_.swap)

  //val string2Int: String => Option[Vector[NodeID]] = str => Try {
   // Vector(string2IntMap(str))
  //}.toOption


  //assert(string2IntMap.size == int2StringMap.size)


}


