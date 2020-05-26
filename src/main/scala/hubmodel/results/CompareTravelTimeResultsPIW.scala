package hubmodel.results

import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import hubmodel.io.input.JSONReaders.ODGroup_JSON
import hubmodel.parseConfigFile
import myscala.math.stats.{ComputeQuantiles, ComputeStats}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import org.apache.commons.math3.distribution.TDistribution
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource
import scala.jdk.CollectionConverters._
import org.apache.commons.math3.stat.inference.TTest


object CompareTravelTimeResultsPIW extends App {

  type TT = Double
  type TD = Double
  type Speed = Double
  type Size = Int

  val config: Config = parseConfigFile(args)

  val odGroups: Option[Map[String, Vector[(String, String)]]] = if (!config.getIsNull("output.OD-groups")) {
    val source: BufferedSource = scala.io.Source.fromFile(config.getString("output.OD-groups"))
    val input: JsValue = Json.parse(try source.mkString finally source.close)
    input.validate[Vector[ODGroup_JSON]] match {
      case s: JsSuccess[Vector[ODGroup_JSON]] => {
        Some(s.get.map(r => r.name -> r.ods.map(v => (v.o, v.d))).toMap)
      }
      case e: JsError => throw new Error("Error while parsing od groups: " + JsError.toJson(e).toString())
    }
  } else {
    None
  }

  def groupsReversed(odPairs: (String, String)): String = {
    odGroups match {
      case Some(s) => s.flatMap(g => g._2.map(t => t -> g._1)).getOrElse(odPairs, "other")
      case None => odPairs._1 + "->" + odPairs._2
    }
    //odGroups.get.flatMap(g => g._2.map(t => t -> g._1)).getOrElse(odPairs, "other")
  }

  val demandSets: Vector[String] = {
    if (!config.getIsNull("sim.demand_sets")) {
      val multipleDemandStream = Files.newDirectoryStream(Paths.get(config.getString("files_1.demand_sets")), "*.json")
      val folders: Vector[String] = multipleDemandStream.asScala.toVector.map(_.getFileName.toString.split("\\.").head)
      multipleDemandStream.close()
      folders
    } else {
      Vector()
    }
  }

  val resultsRef: Vector[ResultsContainerReadWithDemandSetNew] = readResultsJson(config.getString("files_1.dir"), config.getString("files_1.output_prefix"), demandSets).toVector
  val resultsOther: Vector[ResultsContainerReadWithDemandSetNew] = readResultsJson(config.getString("files_2.dir"), config.getString("files_2.output_prefix"), demandSets).toVector


  def resultsByOD(results: Vector[ResultsContainerReadWithDemandSetNew]): Map[String, Map[String, (TT, Double, TD, Speed, Size)]] = {
    val groupedFiles = results
      .zipWithIndex
      .groupBy(_._1.demandFile)

    groupedFiles
      .map(r => {

        // data for KPI for one simulation
        val temp = r._2.map(
          sim => sim._1.tt // using travel time
            .groupBy(p =>  (p.o, p.d)) // grouped by OD pair
          //.map(rr => (rr._1, (rr._2.map(_._3).statistics.median, rr._2.map(_._6).statistics.median, rr._2.map(p => p._6/p._3).statistics.median, rr._2.size))).toVector
        )

        r._1 -> temp.flatten
          .groupBy(g => groupsReversed(g._1))
          .map(group => {
            val dataByGroup = group._2.flatMap(d => d._2)

            val ttData = dataByGroup.map(d => d.tt).statistics

            group._1 -> (
              ttData.median,
              ttData.variance,
              dataByGroup.map(d => d.td).statistics.median,
              dataByGroup.map(d => d.tt / d.td).statistics.median,
              dataByGroup.size
            )
          })
      })
  }

  val resultsByODRef: Map[String, Map[String, (TT, Double, TD, Speed, Size)]] = resultsByOD(resultsRef)
  val resultsByODOther: Map[String, Map[String, (TT, Double, TD, Speed, Size)]] = resultsByOD(resultsOther)


  def WelchTTest(m1: Double, sd1:Double, size1: Int, m2: Double, sd2:Double, size2: Int): (Double, Double) = {
    val nu = math.pow(sd1*sd1/size1 + sd2*sd2/size2, 2) / (math.pow(sd1, 4) / (size1*size1*(size1-1.0)) + math.pow(sd2, 4) / (size2*size2*(size2-1.0)))
    val t = (m1 - m2)/math.sqrt(sd1*sd1/size1 + sd2*sd2 / size2)
    if (nu <0) {
      println("debug")
    }
    (t,nu)
  }


  val resultsCompared: Map[String, Seq[(String, Double, Double, Double, Double, Double, Double, Double, Double, Int, Int)]] =
    resultsByODRef.map(rr => rr._1 -> {
      (
        rr._2
          .map(ref => (ref._1, ref._2, resultsByODOther(rr._1).getOrElse(ref._1, (Double.NaN, Double.NaN, Double.NaN, Double.NaN, 0)))) ++
          resultsByODOther(rr._1)
            .filterNot(kv => rr._2.keySet.contains(kv._1)).map(other => (other._1, (Double.NaN, Double.NaN, Double.NaN, Double.NaN, 0), other._2))
        ).map(r => (r._1, r._2._1, r._3._1, r._2._2, r._3._2, r._2._3, r._3._3, r._2._4, r._3._4, r._2._5, r._3._5)).toVector
    })

  /*resultsCompared.head._2.sortBy(_._1).foreach(r => {
    val (t, nu) = WelchTTest(r._2, r._4, r._10, r._3, r._5, r._11)
    println(r._1, t, nu, new TDistribution(nu).density(t))
  })*/

  resultsCompared.flatMap(rr => rr._2.map(rrr => (rr._1, rrr._1, rrr._2, rrr._3, {
    val (t, nu) = WelchTTest(rrr._2, rrr._4, rrr._10, rrr._3, rrr._5, rrr._11)
    new TDistribution(nu).density(t)
  }, rrr._6, rrr._7, rrr._8, rrr._9, rrr._10, rrr._11))).toVector
    .filterNot(v => v._3.isNaN || v._4.isNaN)
    .sortBy(v => v._2) //(v._3-v._2)/v._2)//v._1)
    // .filterNot(v => v._9 == "other")
    .zipWithIndex
    .map(v => (v._2, v._1._1, v._1._2, v._1._3, v._1._4 , if (v._1._5 <= 0.05 && v._1._11 >= 20){"sigLarge"} else if (v._1._5 <= 0.05 && v._1._11 < 20) {"sigSmall"} else if (v._1._5 > 0.05 && v._1._11 > 20) {"nonSigLarge"} else {"nonSigSmall"}, v._1._6, v._1._7, v._1._8, v._1._9, v._1._10, v._1._11))
    .writeToCSV(config.getString("files_1.output_prefix") + "_VS_" + config.getString("files_2.output_prefix") + "_walking_time_distributions_by_OD.csv", columnNames = Some(Vector("idx", "demandFile", "odGroup", "refTT", "otherTT", "TTequalMeanPValue", "refTravelDistance", "otherTravelDistance", "refMeanSpeed", "otherMeanSpeed", "refPopulationSize", "otherPopulationSize")), rowNames = None)
}
