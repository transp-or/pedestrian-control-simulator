package hubmodel.results

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import hubmodel.io.input.JSONReaders.{ODGroup_JSON, ODPair_JSON_with_AMW}
import hubmodel.parseConfigFile
import myscala.math.stats.{ComputeQuantiles, ComputeStats, Statistics, computeQuantile}
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import org.apache.commons.math3.distribution.TDistribution
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource
import scala.jdk.CollectionConverters._
import org.apache.commons.math3.stat.inference.TTest
import tools.Time
import tools.math.integration.rectangleIntegration


object CompareTravelTimeResultsPIW extends App {

  type TT = Double
  type TD = Double
  type Speed = Double
  type Size = Int

  val config: Config = parseConfigFile(args)

  val odGroups: Option[Map[String, Vector[(String, String)]]] = if (!config.getIsNull("output.OD-groups")) {
    val source: BufferedSource = scala.io.Source.fromFile(config.getString("output.OD-groups"))
    val input: JsValue = Json.parse(try source.mkString finally source.close)
    (input.validate[Vector[ODGroup_JSON]] match {
      case s: JsSuccess[Vector[ODGroup_JSON]] => {
        Some(s.get.map(r => r.name -> r.ods.map(v => (v.o, v.d))).toMap)
      }
      case e: JsError => throw new Error("Error while parsing od groups: " + JsError.toJson(e).toString())
    }).map(s => s.flatMap(kv => Map(kv._1 -> kv._2, kv._1.split("-TO-").last + "-TO-" +  kv._1.split("-TO-").head -> kv._2.map(od => (od._2, od._1)))))
  } else {
    None
  }

  def groupsReversed(odPairs: (String, String)): String = {
    odGroups match {
      case Some(s) => odPairs._1 + "->" + odPairs._2/*s.flatMap(g => g._2.map(t => t -> g._1)).getOrElse(odPairs, "other")*/
      case None => odPairs._1 + "->" + odPairs._2
    }
    //odGroups.get.flatMap(g => g._2.map(t => t -> g._1)).getOrElse(odPairs, "other")
  }

  def ODPairsSorting(odPair: String): String = {
    odGroups match {
      case Some(s) => s.flatMap(g => g._2.map(t => t._1 + "->" + t._2 -> g._1)).getOrElse(odPair, "other")
      case None => odPair
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

  val resultsRef: Vector[ResultsContainerReadNew] = readResultsJson(config.getString("files_1.dir"), config.getString("files_1.output_prefix"), demandSets).toVector
  val resultsOther: Vector[ResultsContainerReadNew] = readResultsJson(config.getString("files_2.dir"), config.getString("files_2.output_prefix"), demandSets).toVector


  val mapping: String => String = str => Map(
    "entrance-bottom-corr-right-top" ->    "entrance-bottom-corr-right",
    "entrance-bottom-corr-right-middle" -> "entrance-bottom-corr-right",
    "entrance-bottom-corr-right-bottom" -> "entrance-bottom-corr-right",
    "entrance-bottom-corr-left-top" ->    "entrance-bottom-corr-left",
    "entrance-bottom-corr-left-middle" -> "entrance-bottom-corr-left",
    "entrance-bottom-corr-left-bottom" -> "entrance-bottom-corr-left",
    "entrance-top-corr-right-top" ->    "entrance-top-corr-right",
    "entrance-top-corr-right-middle" -> "entrance-top-corr-right",
    "entrance-top-corr-right-bottom" -> "entrance-top-corr-right",
    "entrance-top-corr-left-top" ->    "entrance-top-corr-left",
    "entrance-top-corr-left-middle" -> "entrance-top-corr-left",
    "entrance-top-corr-left-bottom" -> "entrance-top-corr-left",
    "plat1-a" -> "plat1-top",
    "plat1-b" -> "plat1-top",
    "plat1-c" -> "plat1-top",
    "plat1-d" -> "plat1-middle",
    "plat1-e" -> "plat1-middle",
    "plat1-f" -> "plat1-middle",
    "plat1-g" -> "plat1-middle",
    "plat1-h" -> "plat1-middle",
    "plat1-i" -> "plat1-middle",
    "plat1-j" -> "plat1-bottom",
    "plat1-k" -> "plat1-bottom",
    "plat1-l" -> "plat1-bottom",
    "plat2-a" -> "plat2-top",
    "plat2-b" -> "plat2-top",
    "plat2-c" -> "plat2-top",
    "plat2-d" -> "plat2-middle",
    "plat2-e" -> "plat2-middle",
    "plat2-f" -> "plat2-middle",
    "plat2-g" -> "plat2-middle",
    "plat2-h" -> "plat2-middle",
    "plat2-i" -> "plat2-middle",
    "plat2-j" -> "plat2-bottom",
    "plat2-k" -> "plat2-bottom",
    "plat2-l" -> "plat2-bottom",
    "plat3-a" -> "plat3-top",
    "plat3-b" -> "plat3-top",
    "plat3-c" -> "plat3-top",
    "plat3-d" -> "plat3-middle",
    "plat3-e" -> "plat3-middle",
    "plat3-f" -> "plat3-middle",
    "plat3-g" -> "plat3-middle",
    "plat3-h" -> "plat3-middle",
    "plat3-i" -> "plat3-middle",
    "plat3-j" -> "plat3-bottom",
    "plat3-k" -> "plat3-bottom",
    "plat3-l" -> "plat3-bottom"
  ).getOrElse(str, str)



  def TTresultsByOD(results: Vector[ResultsContainerReadNew]): Map[String, Map[String, (TT, Double, TD, Speed, Size)]] = {
    val (noDemandSets, withDemandSets) = (results.partitionMap {
      case b: ResultsContainerReadWithDemandSetNew => Right(b.asInstanceOf[ResultsContainerReadWithDemandSetNew])
      case a: ResultsContainerReadNew => Left({
        new ResultsContainerReadNew(a.id, a.tt.map(tt => tt.copy(o = mapping(tt.o), d = mapping(tt.d))), a.monitoredAreaDensity, a.amwData)
      })
    })


    val groupedFiles: Map[String, Vector[(ResultsContainerReadNew, Int)]] = {
      if (noDemandSets.nonEmpty && withDemandSets.isEmpty) {
        Map("all" -> noDemandSets.zipWithIndex)
      }
      else {withDemandSets
        .zipWithIndex
        .groupBy(_._1.demandFile)}
    }

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
              ttData.mean,
              ttData.variance,
              dataByGroup.map(d => d.td).statistics.mean,
              dataByGroup.map(d => d.tt / d.td).statistics.mean,
              dataByGroup.size
            )
          })
      })
  }




  /*def computeDensityIntegral(individiualDensityData: Map[(String, String), Vector[(Double, Vector[Double])]]): Double = {
    rectangleIntegration(individiualDensityData.toVector.flatMap(a => a._2.map(d => d._1 * {if (d._2.isEmpty){0.0} else {math.max(0.0, computeQuantile(75)(d._2).value - 1.08)}})), , )
  }*/

  def computeDensityQuantile(individiualDensityData: Map[(String, String), Vector[(Double, Vector[Double])]]): Map[(String, String), Vector[(Double, Double)]] = {
    individiualDensityData.view.mapValues(a => a.map(d => (d._1, if (d._2.isEmpty) {0.0} else {computeQuantile(75)(d._2).value}))).toMap
  }

  val resultsByODRef: Map[String, Map[String, (TT, Double, TD, Speed, Size)]] = TTresultsByOD(resultsRef)
  val resultsByODOther: Map[String, Map[String, (TT, Double, TD, Speed, Size)]] = TTresultsByOD(resultsOther)

  def densityResultsByZone(results: Vector[ResultsContainerReadNew]): Vector[(String, Vector[(Double, Double)])] = {
    results.collect{
        case r: ResultsContainerReadNew if r.monitoredAreaDensity.isDefined => "all" -> computeDensityQuantile(r.monitoredAreaDensity.get.map(d => (d._1, d._2.id) -> d._2.disaggregateMeasurements))
      case r: ResultsContainerReadWithDemandSetNew if r.monitoredAreaDensity.isDefined => r.demandFile -> computeDensityQuantile(r.monitoredAreaDensity.get.map(d => (d._1, d._2.id) -> d._2.disaggregateMeasurements))
    }.flatMap(v => v._2.map(vv => v._1 + "_" + vv._1._1 -> vv._2))
  }

  /*val resultsDensityRef: Map[String, Statistics[Double]] = resultsRef
    .collect{
      case r: ResultsContainerReadNew if r.monitoredAreaIndividualDensity.isDefined => "all" -> r.monitoredAreaDensity.
      case r:ResultsContainerReadWithDemandSetNew if r.monitoredAreaIndividualDensity.isDefined => r.demandFile -> computeDensityIntegral(r.monitoredAreaIndividualDensity.get)
    }
    .groupBy(_._1)
    .view
    .mapValues(v => v.map(_._2).statistics)
    .map(kv => kv._1 + "_ref" -> kv._2)
    .to(Map)

  val resultsDensityOther: Map[String, Statistics[Double]] = resultsOther
    .collect{
      case r: ResultsContainerReadNew if r.monitoredAreaIndividualDensity.isDefined => "all" -> computeDensityIntegral(r.monitoredAreaIndividualDensity.get)
      case r:ResultsContainerReadWithDemandSetNew if r.monitoredAreaIndividualDensity.isDefined => r.demandFile -> computeDensityIntegral(r.monitoredAreaIndividualDensity.get)
    }    .groupBy(_._1)
    .view
    .mapValues(v => v.map(_._2).statistics)
    .map(kv => kv._1 + "_other" -> kv._2)
    .to(Map)

  (resultsDensityRef ++ resultsDensityOther).toVector.map(kv => (kv._1, kv._2.toCSV))
    .writeToCSV("density-integral-stats.csv", columnNames=Some(Vector("name") ++ resultsDensityRef.head._2.CSVColumnNames.split(",")), rowNames = None)
*/
  /*val densityResultsByZoneRef: Vector[(String, Vector[(Time, Double)])] = densityResultsByZone(resultsRef).sortBy(_._1)
  val densityResultsByZoneOther: Vector[(String, Vector[(Time, Double)])] = densityResultsByZone(resultsOther).sortBy(_._1)

  val headersDensity = densityResultsByZoneRef.flatMap(v => Vector(v._1 + "_t", v._1 + "_d"))

  densityResultsByZoneRef.flatMap(v => Vector(v._2.map(_._1.value.toDouble), v._2.map(_._2))).writeToCSV("density-over-time-" + config.getString("files_1.output_prefix") + ".csv", columnNames = Some(headersDensity), rowNames = None)
  densityResultsByZoneOther.flatMap(v => Vector(v._2.map(_._1.value.toDouble), v._2.map(_._2))).writeToCSV("density-over-time-" + config.getString("files_2.output_prefix") + ".csv", columnNames = Some(headersDensity), rowNames = None)


  val avergaeDensityResultsByZoneRef: Vector[(String, Vector[(Time, Double)])] = densityResultsByZoneRef.groupBy(_._1)
    .map(kv => (kv._1, kv._2.flatMap(td => td._2).groupBy(_._1).map(d => d._1 -> d._2.map(_._2).sum / d._2.size).toVector.sortBy(_._1))).toVector.sortBy(_._1)

  val avergaeDensityResultsByZoneOther: Vector[(String, Vector[(Time, Double)])] = densityResultsByZoneOther.groupBy(_._1)
    .map(kv => (kv._1, kv._2.flatMap(td => td._2).groupBy(_._1).map(d => d._1 -> d._2.map(_._2).sum / d._2.size).toVector.sortBy(_._1))).toVector.sortBy(_._1)

  val headersDensityAverage = avergaeDensityResultsByZoneRef.flatMap(v => Vector(v._1 + "_t", v._1 + "_d"))
  avergaeDensityResultsByZoneRef.flatMap(v => Vector(v._2.map(_._1.value.toDouble), v._2.map(_._2))).writeToCSV("average-density-over-time-" + config.getString("files_1.output_prefix") + ".csv", columnNames = Some(headersDensityAverage), rowNames = None)
  avergaeDensityResultsByZoneOther.flatMap(v => Vector(v._2.map(_._1.value.toDouble), v._2.map(_._2))).writeToCSV("average-density-over-time-" + config.getString("files_2.output_prefix") +".csv", columnNames = Some(headersDensityAverage), rowNames = None)
*/

  def WelchTTest(m1: Double, sd1:Double, size1: Int, m2: Double, sd2:Double, size2: Int): (Double, Double) = {
    val nu = math.pow(sd1*sd1/size1 + sd2*sd2/size2, 2) / (math.pow(sd1, 4) / (size1*size1*(size1-1.0)) + math.pow(sd2, 4) / (size2*size2*(size2-1.0)))
    val t = (m1 - m2)/math.sqrt(sd1*sd1/size1 + sd2*sd2 / size2)
    (t,nu)
  }

  def labelling(pValue: Double, countRef: Int, countOther: Int): String = {
    val largeIf: Int = 5
    if (countRef >= largeIf * this.resultsRef.size && countOther >= largeIf * this.resultsOther.size){
      if (pValue < 0.05) {"sigLarge"}
      else {"nonSigLarge"}
    } else {
      if (pValue < 0.05) {"sigSmall"}
      else {"nonSigSmall"}
    }
  }


  lazy val amwCountByOD: Map[String, Int] = {
    val source: BufferedSource = scala.io.Source.fromFile(config.getString("sim.amws_by_OD"))
    val input: JsValue = Json.parse(try source.mkString finally source.close)
    input.validate[Vector[ODPair_JSON_with_AMW]] match {
      case s: JsSuccess[Vector[ODPair_JSON_with_AMW]] => {

        /*{
          val file = new File("od-groups-by-amws-count.json")
          val bw = new BufferedWriter(new FileWriter(file))
          bw.write("[{")

          s.get
            .map(r => (r.o, r.d, r.amws.size))
            .groupBy(_._3)
            .foreach(g => {
              bw.write("\"name\":" + "\"" + g._1 + "\",\n\"ods\": [" )
              bw.write(g._2.map(od => {
                "{" + "\"o\":\"" + od._1 + "\", \"d\":\"" + od._2 + "\"}"
              }).mkString(",\n"))
              bw.write("]},\n{")
            })
          bw.write("}]")
          bw.close()
        }*/

        s.get.map(r => mapping(r.o) + "->" + mapping(r.d) -> r.amws.size).toMap
      }
      case e: JsError => throw new Error("Error while parsing amws by ods: " + JsError.toJson(e).toString())
    }
  }



  val resultsTTCompared: Map[String, Seq[(String, Double, Double, Double, Double, Double, Double, Double, Double, Int, Int)]] =
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

  // walking time distribution comparison
  resultsTTCompared.flatMap(rr => rr._2.map(rrr => (rr._1, rrr._1, rrr._2, rrr._3, {
    val (t, nu) = WelchTTest(rrr._2, rrr._4, rrr._10, rrr._3, rrr._5, rrr._11)
    new TDistribution(nu).density(t)
  }, rrr._6, rrr._7, rrr._8, rrr._9, rrr._10, rrr._11))).toVector
    .filterNot(v => (v._3.isNaN || v._4.isNaN))
    //.filter(v => amwCountByOD.keySet.contains(v._2))
    //.sortBy(v => {amwCountByOD(v._2)}) //(v._3-v._2)/v._2)//v._1)
    // .filterNot(v => v._9 == "other")
    //.map(v => {if (v._2.contains("7") || v._2.contains("8")){0} else {1}})
    .sortBy(v => ODPairsSorting(v._2) + v._2)
    .zipWithIndex
    .map(v => (
      v._2, v._1._1, v._1._2, v._1._3, v._1._4,
      100.0*(v._1._4-v._1._3)/v._1._3,
      labelling(v._1._5, v._1._10, v._1._11),
      v._1._6, v._1._7, v._1._8, v._1._9, v._1._10, v._1._11, v._1._5, ODPairsSorting(v._1._2)
    ))
    .writeToCSV(config.getString("files_1.output_prefix") + "_VS_" + config.getString("files_2.output_prefix") + "_walking_time_distributions_by_OD.csv", columnNames = Some(Vector("idx", "demandFile", "odGroup", "refTT", "otherTT","relativeTTDiff" , "TTequalMeanPValue", "refTravelDistance", "otherTravelDistance", "refMeanSpeed", "otherMeanSpeed", "refPopulationSize", "otherPopulationSize", "pvalue", "MISC")), rowNames = None)


}
