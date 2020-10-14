import java.io.{BufferedWriter, File, FileWriter}

import hubmodel.demand.Pedestrian_JSON
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import tools.Time

import scala.io.BufferedSource
import scala.util.Random

object SimplyDisaggOD extends App {

  val file: String = "piw-corridor/piw-data/lausanne-20130122-piw-disggregate-demand.json"

  val zoneMapping: Map[String, Vector[String]] = Map(
    "1" -> Vector("entrance-top-corr-left"),
    "2" -> Vector("entrance-top-corr-left"),
    "3" -> Vector("entrance-top-corr-left"),
    "4" -> Vector("entrance-top-corr-left"),
    "5" -> Vector("plat1-a", "plat1-b", "plat1-c","plat1-d"),
    "6" -> Vector("plat1-d", "plat1-e", "plat1-f","plat1-g", "plat1-h","plat1-i"),
    "7" -> Vector("plat2-a", "plat2-b", "plat2-c","plat2-d"),
    "8" -> Vector("plat2-d", "plat2-e", "plat2-f","plat2-g", "plat2-h","plat2-i"),
    "9" -> Vector("plat3-a", "plat3-b", "plat3-c","plat3-d"),
    "10" ->Vector("plat3-d", "plat3-e", "plat3-f","plat3-g", "plat3-h","plat3-i"),
    "11" ->Vector("entrance-top-corr-right"),
    "12" -> Vector("entrance-top-corr-right"),
    "13" -> Vector("entrance-top-corr-right"),
    "14" -> Vector("entrance-top-corr-right"),
    "15" -> Vector("entrance-bottom-corr-left"),
    "16" -> Vector("entrance-bottom-corr-left"),
    "17" -> Vector("plat2-d", "plat2-e", "plat2-f","plat2-g", "plat2-h","plat2-i"),
    "18" -> Vector("plat2-i", "plat2-j", "plat2-k","plat2-l"),
    "19" -> Vector("plat3-d", "plat3-e", "plat3-f","plat3-g", "plat3-h","plat3-i"),
    "20" -> Vector("plat3-i", "plat3-j", "plat3-k","plat3-l"),
    "21" -> Vector("entrance-bottom-corr-right"),
    "22" -> Vector("entrance-bottom-corr-right"),
    "23" -> Vector("entrance-bottom-corr-right")
  )

    val pop: Vector[Pedestrian_JSON] = {
      val source: BufferedSource = scala.io.Source.fromFile(file)
      val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Vector[Pedestrian_JSON]] match {
      case s: JsSuccess[Vector[Pedestrian_JSON]] => s.get
      case e: JsError => throw new Error("Error while parsing disaggregate pedestrian: " + JsError.toJson(e).toString())
    }
  }

  val newPop = pop.flatMap(ped => {
    val o: String = ped.oZone.split("_").last
    val d: String = ped.dZone.split("_").last
    if (o == "-1" ||d == "-1") {
      None
    } else {
      Some(ped.copy(oZone = "z_" + zoneMapping(o)(Random.nextInt(zoneMapping(o).size)), dZone = "z_" + zoneMapping(d)(Random.nextInt(zoneMapping(d).size))))
    }
  })

  val oFile = new File("lausanne-20130122-piw-disaggregate-demand-three-platforms-OD.json")
  val bw = new BufferedWriter(new FileWriter(oFile))
  bw.write(Json.prettyPrint(Json.toJson(newPop)))
  bw.close()

}
