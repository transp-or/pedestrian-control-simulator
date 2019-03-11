package hubmodel.results

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.{ComputeStats, Statistics}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import scala.collection.JavaConversions._


object CompareTravelTimeResults extends App {

  // Groups per direction for PIW corridor
  val groups: Map[String, Vector[(String, String)]] = Map(
    "nonCrossShort" -> Vector(
      ("11", "9"),
      ("9", "7"),
      ("7", "5"),
      ("6", "8"),
      ("8", "10"),
      ("10", "12"),
    ),
    "crossShort" -> Vector(
      ("9", "11"),
      ("7", "9"),
      ("5", "7"),
      ("8", "6"),
      ("10", "8"),
      ("12", "10"),
    ),
    "nonCrossLong" -> Vector(
      ("11", "7"),
      ("9", "5"),
      ("6", "10"),
      ("8", "12"),
      ("11", "5"),
      ("6", "12")
    ),
    "crossLong" -> Vector(
      ("7", "11"),
      ("5", "9"),
      ("10", "6"),
      ("12", "8"),
      ("12", "6"),
      ("5", "11")
    ))

  val config: Config = parseConfigFile(args)
  val multipleDemandStream = Files.newDirectoryStream(Paths.get(config.getString("sim.TF_demand_sets")), "*.json")
  val subFolders: Vector[String] = multipleDemandStream.toVector.map(_.getFileName.toString.split("\\.").head)
  multipleDemandStream.close()

  val resultsRef: Vector[ResultsContainerReadWithDemandSet] = readResults(config.getString("files_1.dir"), config.getString("files_1.output_prefix"), subFolders).toVector
  val resultsOther: Vector[ResultsContainerReadWithDemandSet] = readResults(config.getString("files_2.dir"), config.getString("files_2.output_prefix"), subFolders).toVector

  val resultsByODRef: Map[String, Map[String, (Statistics[Double], String)]] = resultsRef.zipWithIndex.groupBy(_._1.demandFile).map(r => {
    r._1 -> r._2.flatMap(rm => rm._1.tt)
    .groupBy(p => (p._1, p._2))
    .map(rm => rm._1._1 + "->" + rm._1._2 -> (rm._2.map(_._3).statistics, {val g = groups.find(g => g._2.contains((rm._1._1, rm._1._2))); if(g.isDefined){g.get._1} else{"non"}}))
  })

  val resultsByODOther: Map[String, Map[String, (Statistics[Double], String)]] = resultsOther.zipWithIndex.groupBy(_._1.demandFile).map(r => {
    r._1 -> r._2.flatMap(rm => rm._1.tt)
      .groupBy(p => (p._1, p._2))
      .map(rm => rm._1._1 + "->" + rm._1._2 -> (rm._2.map(_._3).statistics, {val g = groups.find(g => g._2.contains((rm._1._1, rm._1._2))); if(g.isDefined){g.get._1} else{"non"}}))
  })

  val r: Map[String, Seq[(String, Double, Double, Int, Int, String)]] =
    resultsByODRef.map(rr => rr._1 -> {
      (
        rr._2
          .map(ref => (ref._1, ref._2, resultsByODOther(rr._1).getOrElse(ref._1, (Statistics(0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN), "non")))) ++
          resultsByODOther(rr._1)
            .filterNot(kv => rr._2.keySet.contains(kv._1)).map(other => (other._1, (Statistics(0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN), "non"), other._2))
        ).map(r => (r._1, r._2._1.median, r._3._1.median, r._2._1.size, r._3._1.size, r._2._2)).toVector
    })


  r.flatMap(rr => rr._2.map(rrr => (rr._1, rrr._1, rrr._2, rrr._3, rrr._4, rrr._5, rrr._6))).toVector
    .filterNot(v => v._3.isNaN || v._4.isNaN)
    .sortBy(v => v._7).reverse//(v._3-v._2)/v._2)//v._1)
    .filterNot(v => v._7 == "non")
    .zipWithIndex
    .map(v => (v._2, v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6, if (v._1._4 - v._1._3 < 0.0) {
      "neg"
    } else {
      "pos"
    },
    v._1._7))
    .writeToCSV(config.getString("files_1.output_prefix") + "_VS_" + config.getString("files_2.output_prefix") + "_walking_time_distributions_by_OD.csv", columnNames = Some(Vector("idx", "demandFile","od", "refTT", "otherTT", "refSize", "otherSize", "signOtherMinusRef", "odGroup")), rowNames = None)
}
