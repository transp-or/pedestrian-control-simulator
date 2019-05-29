package hubmodel.results

import java.nio.file.{Files, Paths}
import com.typesafe.config.Config
import hubmodel.io.input.JSONReaders.ODGroup_JSON
import hubmodel.parseConfigFile
import myscala.math.stats.{ComputeQuantiles, ComputeStats}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.collection.JavaConversions._
import scala.io.BufferedSource


object CompareTravelTimeResultsPIW extends App {

  val config: Config = parseConfigFile(args)

  val odGroups: Map[String, Vector[(String, String)]] = {
    val source: BufferedSource = scala.io.Source.fromFile(config.getString("output.OD-groups"))
    val input: JsValue = Json.parse(try source.mkString finally source.close)
    input.validate[Vector[ODGroup_JSON]] match {
      case s: JsSuccess[Vector[ODGroup_JSON]] => {
        s.get.map(r => r.name -> r.ods.map(v => (v.o, v.d))).toMap
      }
      case e: JsError => throw new Error("Error while parsing od groups: " + JsError.toJson(e).toString())
    }
  }

  def groupsReversed(odPairs: (String, String)): String = odGroups.flatMap(g => g._2.map(t => t -> g._1)).getOrElse(odPairs, "other")

  val multipleDemandStream = Files.newDirectoryStream(Paths.get(config.getString("sim.TF_demand_sets")), "*.json")
  val subFolders: Vector[String] = multipleDemandStream.toVector.map(_.getFileName.toString.split("\\.").head)
  multipleDemandStream.close()

  val resultsRef: Vector[ResultsContainerReadWithDemandSet] = readResults(config.getString("files_1.dir"), config.getString("files_1.output_prefix"), subFolders).toVector
  val resultsOther: Vector[ResultsContainerReadWithDemandSet] = readResults(config.getString("files_2.dir"), config.getString("files_2.output_prefix"), subFolders).toVector


  def resultsByOD(results: Vector[ResultsContainerReadWithDemandSet]): Map[String, Map[String, (Double, Double, Double, Int)]] = {
    val groupedFiles = results
      .zipWithIndex
      .groupBy(_._1.demandFile)

    groupedFiles
      .map(r => {

        // data for KPI for one simulation
        val temp = r._2.map(
          sim => sim._1.tt // using travel time
            .groupBy(p => (p._1, p._2)) // grouped by OD pair
          //.map(rr => (rr._1, (rr._2.map(_._3).statistics.median, rr._2.map(_._6).statistics.median, rr._2.map(p => p._6/p._3).statistics.median, rr._2.size))).toVector
        )

        r._1 -> temp.flatten
          .groupBy(g => groupsReversed(g._1))
          .map(group => {
            val dataByGroup = group._2.flatMap(d => d._2)
            group._1 -> (
              dataByGroup.map(d => d._3).cutOfAfterQuantile(99.5).statistics.median,
              dataByGroup.map(d => d._6).cutOfAfterQuantile(99.5).statistics.median,
              dataByGroup.map(d => d._6 / d._3).cutOfAfterQuantile(99.5).statistics.median,
              dataByGroup.size
            )
          })
      })
  }

  val resultsByODRef: Map[String, Map[String, (Double, Double, Double, Int)]] = resultsByOD(resultsRef)
  val resultsByODOther: Map[String, Map[String, (Double, Double, Double, Int)]] = resultsByOD(resultsOther)


  val r: Map[String, Seq[(String, Double, Double, Double, Double, Double, Double, Int, Int)]] =
    resultsByODRef.map(rr => rr._1 -> {
      (
        rr._2
          .map(ref => (ref._1, ref._2, resultsByODOther(rr._1).getOrElse(ref._1, (Double.NaN, Double.NaN, Double.NaN, 0)))) ++
          resultsByODOther(rr._1)
            .filterNot(kv => rr._2.keySet.contains(kv._1)).map(other => (other._1, (Double.NaN, Double.NaN, Double.NaN, 0), other._2))
        ).map(r => (r._1, r._2._1, r._3._1, r._2._2, r._3._2, r._2._3, r._3._3, r._2._4, r._3._4)).toVector
    })


  r.flatMap(rr => rr._2.map(rrr => (rr._1, rrr._1, rrr._2, rrr._3, rrr._4, rrr._5, rrr._6, rrr._7, rrr._8, rrr._9))).toVector
    .filterNot(v => v._3.isNaN || v._4.isNaN)
    .sortBy(v => v._2) //(v._3-v._2)/v._2)//v._1)
    // .filterNot(v => v._9 == "other")
    .zipWithIndex
    .map(v => (v._2, v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6, v._1._7, v._1._8, v._1._9, v._1._10))
    .writeToCSV(config.getString("files_1.output_prefix") + "_VS_" + config.getString("files_2.output_prefix") + "_walking_time_distributions_by_OD.csv", columnNames = Some(Vector("idx", "demandFile", "odGroup", "refTT", "otherTT", "refTravelDistance", "otherTravelDistance", "refMeanSpeed", "otherMeanSpeed", "refPopulationSize", "otherPopulationSize")), rowNames = None)
}
