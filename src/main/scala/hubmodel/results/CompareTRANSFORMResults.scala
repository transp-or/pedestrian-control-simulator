package hubmodel.results

import com.typesafe.config.Config
import hubmodel.supply.graph.readPTStop2GraphVertexMap
import hubmodel.tools.Time
import hubmodel.{ResultsContainerRead, parseConfigFile, readResults}
import hubmodel.output.TRANSFORM.PopulationSummaryProcessingTRANSFORM
import myscala.math.stats.ComputeStats
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter


object CompareTRANSFORMResults extends App  {

  val config: Config = parseConfigFile(args)
  val simulationStartTime: Time = Time(config.getDouble("sim.start_time"))
  val simulationEndTime: Time = Time(config.getDouble("sim.end_time"))
  val stop2Vertex = readPTStop2GraphVertexMap(config.getString("sim.zones_to_vertices_map"))


  val resultsRef: Vector[ResultsContainerRead] = readResults(config.getString("files_1.dir"), config.getString("files_1.output_prefix")).toVector
  val resultsOther: Vector[ResultsContainerRead] = readResults(config.getString("files_2.dir"), config.getString("files_2.output_prefix")).toVector

  val resultsByODRef = resultsRef
    .flatten(_.tt)
    .computeTT4TRANSFORM(0.0.to(100.0).by(2.0), simulationStartTime, simulationEndTime, config.getString("files_1.write_tt_4_transform_file_name"), stop2Vertex)
    .map(r => r._1 + "->" + r._2 -> r._3.stats).toMap

  val resultsByODOther = resultsOther
    .flatten(_.tt)
    .computeTT4TRANSFORM(0.0.to(100.0).by(2.0), simulationStartTime, simulationEndTime, config.getString("files_2.write_tt_4_transform_file_name"), stop2Vertex)
    .map(r => r._1 + "->" + r._2 -> r._3.stats).toMap


  val r: Seq[(String, Double, Double)] = (
    resultsByODRef
    .map(ref => (ref._1, ref._2, resultsByODOther.getOrElse(ref._1, (0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN)))) ++
    resultsByODOther
      .filterNot(kv => resultsByODRef.keySet.contains(kv._1)).map(other => (other._1, (0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN), other._2))
    ).map(r => (r._1, r._2._4, r._3._4)).toVector

    r.filterNot(v => v._2.isNaN || v._3.isNaN)
      .sortBy(_._1)
      .zipWithIndex
      .map(v => (v._2, v._1._1, v._1._2, v._1._3))
      .writeToCSV(config.getString("files_1.output_prefix") + "_VS_" + config.getString("files_2.output_prefix") +  "_walking_time_distributions_by_OD.csv", columnNames=Some(Vector("idx", "od", "ref", "other")), rowNames=None)
}
