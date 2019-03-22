package trackingdataanalysis

import java.util.concurrent.ThreadLocalRandom

import hubmodel.demand.{Pedestrian_JSON}
import hubmodel.tools.Time
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter


import scala.io.BufferedSource

object WriteTravelTime extends App {

  val fileName = "lausanne-20130430-piw-disggregate-demand.json"

  val peds: Vector[Pedestrian_JSON] = {
    val source: BufferedSource = scala.io.Source.fromFile("/home/nicholas/PhD/code/hub-simulator/piw-corridor/piw-data/" + fileName)
    val input: JsValue = Json.parse(try source.mkString finally source.close)

    input.validate[Vector[Pedestrian_JSON]] match {
      case s: JsSuccess[Vector[Pedestrian_JSON]] => s.get
      case e: JsError => throw new Error("Error while parsing disaggregate pedestrian: " + JsError.toJson(e).toString())
    }
  }

    peds
      .map(p => (p.oZone.replaceFirst("z_", ""), p.dZone.replaceFirst("z_", ""), p.exitTime - p.entryTime, p.entryTime, p.exitTime, Double.NaN))
      .writeToCSV("piw-empirical-data_tt_" + fileName + ".csv", rowNames = None, columnNames = Some(Vector("origin", "destination", "travelTime", "entryTime", "exitTime", "travelDistance")), "/home/nicholas/PhD/code/hub-simulator/piw-corridor/piw-data-summary/")
}
