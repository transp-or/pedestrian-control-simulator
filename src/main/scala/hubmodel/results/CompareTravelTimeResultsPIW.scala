package hubmodel.results

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.Config
import hubmodel.parseConfigFile
import myscala.math.stats.{ComputeStats, Statistics, ComputeQuantiles}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import scala.collection.JavaConversions._


object CompareTravelTimeResultsPIW extends App {

  // Groups per direction for PIW corridor
  val groups: Map[String, Vector[(String, String)]] = Map(
    "acrossShort" -> Vector(
      ("9", "11"),
      ("7", "9"),
      ("5", "7"),
      ("8", "6"),
      ("10", "8"),
      ("12", "10"),
    ),
    "bcrossLong" -> Vector(
      ("7", "11"),
      ("5", "9"),
      ("10", "6"),
      ("12", "8"),
    ),
    "ccrossVeryLong" -> Vector(
      ("12", "6"),
      ("5", "11")
    ),
    "dnonCrossShort" -> Vector(
      ("11", "9"),
      ("9", "7"),
      ("7", "5"),
      ("6", "8"),
      ("8", "10"),
      ("10", "12"),
    ),
    "enonCrossLong" -> Vector(
      ("11", "7"),
      ("9", "5"),
      ("6", "10"),
      ("8", "12")
    ),
    "fnonCrossVeryLong" -> Vector(
      ("11", "5"),
      ("6", "12")
    ),
    "gcrossSideShort" -> Vector(
      ("5", "8"),
      ("7", "10"),
      ("9", "12"),
      ("8", "5"),
      ("10", "7"),
      ("12", "9"),
      ("9", "8"),
      ("8", "9"),
      ("7", "6"),
      ("6", "7"),
      ("10", "11"),
      ("11", "10")
    ),
    "hcrossSideLong" -> Vector(
      ("5", "10"),
      ("10", "5"),
      ("7", "12"),
      ("12", "7"),
      ("11", "8"),
      ("8", "11"),
      ("9", "6"),
      ("6", "9")
    ),
    "icrossSideVeryLong" -> Vector(
      ("12", "5"),
      ("5", "12"),
      ("11", "6"),
      ("6", "11"),
    ))

  def groupsReversed(odPairs: (String, String)): String = groups.flatMap(g => g._2.map(t => t -> g._1)).getOrElse(odPairs, "xNone")

  val config: Config = parseConfigFile(args)
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
                dataByGroup.map(d => d._6/d._3).cutOfAfterQuantile(99.5).statistics.median,
                dataByGroup.size
              )
            })})
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
    .sortBy(v => v._2)//(v._3-v._2)/v._2)//v._1)
   // .filterNot(v => v._9 == "non")
    .zipWithIndex
    .map(v => (v._2, v._1._1, v._1._2, v._1._3, v._1._4, v._1._5, v._1._6, v._1._7, v._1._8, v._1._9, v._1._10))
    .writeToCSV(config.getString("files_1.output_prefix") + "_VS_" + config.getString("files_2.output_prefix") + "_walking_time_distributions_by_OD.csv", columnNames = Some(Vector("idx", "demandFile","odGroup", "refTT", "otherTT", "refTravelDistance", "otherTravelDistance", "refMeanSpeed", "otherMeanSpeed", "refPopulationSize", "otherPopulationSize")), rowNames = None)
}
