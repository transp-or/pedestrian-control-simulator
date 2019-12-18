package demandestimation

import demandestimation.network.NetworkLausanne
import tools.cells.Vertex

class MeasurementData(parameters: DemandEstimationParameters, network: NetworkLausanne) {

  private val sales_per_minute: Map[String, Double] = Map("KIOSK"-> 1.0, "BAR"-> 1.0, "SHOP"-> 1.0)

  private val sector_dest_flow_fractions: Map[String, Double]  = Map("A"-> 0.095,
    "B"-> 0.271,
    "C"-> 0.475,
    "D"-> 0.159)

  private val centroid_platform_dest_flow_fractions:  Map[String, Double]  = Map("1D"-> 0.25,
    "1C"-> 0.5,
    "70FE"-> 1.0,
    "1AB"-> 0.25,
    "34D"-> sector_dest_flow_fractions("D"),
    "34C"-> sector_dest_flow_fractions("C"),
    "34B"-> sector_dest_flow_fractions("B"),
    "34A"-> sector_dest_flow_fractions("A"),
    "56D"-> sector_dest_flow_fractions("D"),
    "56C"-> sector_dest_flow_fractions("C"),
    "56B"-> sector_dest_flow_fractions("B"),
    "56A"-> sector_dest_flow_fractions("A"),
    "78D"-> sector_dest_flow_fractions("D"),
    "78C"-> sector_dest_flow_fractions("C"),
    "78B"-> sector_dest_flow_fractions("B"),
    "78A"-> sector_dest_flow_fractions("A"),
    "9D"-> 0.75,
    "9C"-> 0.25)




  /**   Loads then processes the data from ASE. The data is processed to make it easily
    usable latter on. Basically so it has the same format as the rest of the framework.
    *
    * @return
    */
  def ASEdataPreprocessing(): Map[(Vertex, Vertex), Vector[Double]] = {


    def csvToMap(file: String): Map[String, Vector[Double]] = {
      val bufferedSource = io.Source.fromFile(file)
      val header = bufferedSource.getLines().iterator.next()
      val colsToLabels: Map[Int, String] = header.split(",").zipWithIndex.map(d => d._2 -> d._1).toMap
      val ASEData: Map[String, collection.mutable.ArrayBuffer[Double]] = header.split(",").map(h => h -> collection.mutable.ArrayBuffer[Double]()).toMap
      for (line <- bufferedSource.getLines) {
        val cols = line.split(",").map(_.trim)
        // do whatever you want with the columns here
        cols.zipWithIndex.map(c => ASEData(colsToLabels(c._2)).append(c._1.toDouble))
        //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
      }
      bufferedSource.close
      ASEData.view.mapValues(_.toVector).toMap
    }




  //************** ASE Data **************
  val ASE_LS: Map[String, Vector[Double]]  = csvToMap(parameters.path_ASE_LS_file).removedAll(Vector("start_day","start_month","start_year","start_hour","start_minute","start_second","end_day","end_month","end_year","end_hour","end_minute","end_second"))
  val ASE_add: Map[String, Vector[Double]]  = csvToMap(parameters.path_ASE_add_file)

  val indicesMeasurementData: Vector[Int] = ASE_add("start_hour").map(_ * 3600).zip(ASE_add("start_minute").map(_ * 60)).map(hm => (hm._1 + hm._2).toInt).zipWithIndex.filter(measurementsTime => parameters.intervalsInt.contains(measurementsTime._1)).map(_._2)

  //Table to rename the edges from the file to the names used in LausNetw
    //self.ASE_edges_conversion = network.ASE_edge_names_dict


  //Edges that we need from ASE.add file:
  //We take the edges that we need and add them in ASE_LS
    var ASEDataAdd: collection.mutable.Map[String, Vector[Double]] = {
      collection.mutable.Map[String, Vector[Double]]() ++=  ASE_LS ++= Vector("ASE1_in", "ASE1_out", "ASE3_in", "ASE3_out", "ASE6_in", "ASE6_out").map(k => k -> ASE_add(k))
    }

  //We merge the counts of the sensors (8a,8b) and (2ab, 2c)
    ASEDataAdd += "ASE8_in" -> ASE_LS("ASE8a_in").zip(ASE_LS("ASE8b_in")).map(t => t._1 + t._2) +=
    "ASE8_out" -> ASE_LS("ASE8a_out").zip(ASE_LS("ASE8b_out")).map(t => t._1 + t._2) +=
    "ASE2_in" -> ASE_LS("ASE2ab_in").zip(ASE_LS("ASE2c_in")).map(t => t._1 + t._2) +=
    "ASE2_out" -> ASE_LS("ASE2ab_out").zip(ASE_LS("ASE2c_out")).map(t => t._1 + t._2)

    //We delete the counts that have been merged
    ASEDataAdd --= Vector("ASE8a_in", "ASE8a_out",    "ASE8b_in",    "ASE8b_out",    "ASE2ab_in",    "ASE2ab_out",    "ASE2c_in",    "ASE2c_out")


  //We rename the edges
    //ASE_LS = ASE_LS.rename(columns=self.ASE_edges_conversion)
   val ASEData: Map[(Vertex, Vertex), Vector[Double]] = ASEDataAdd.map(kv => network.ASE_edge_names_dict(kv._1) -> kv._2).toMap

  /*ASE_LS["start_date") = ASE_LS.start_day.astype(int).map(str) + "-" + ASE_LS.start_month.astype(int).map(str) + "-" + ASE_LS.start_year.astype(int).map(str) + " " + ASE_LS.start_hour.astype(int).map(str) + ":" + ASE_LS.start_minute.astype(int).map(str) + ":" + ASE_LS.start_second.astype(int).map(str)
  ASE_LS["start_date") = ASE_LS["start_date").apply(self.stringToDateTime)
  ASE_LS = ASE_LS.set_index("start_date", drop=True)
  return ASE_LS*/
    ASEData.view.mapValues(v => indicesMeasurementData.map(v(_))).toMap
    }


  val ASEData: Map[(Vertex, Vertex), Vector[Double]] = ASEdataPreprocessing()

  val f_hat: Vector[Double] = (for {
    t <- parameters.intervals.indices
    e <- network.edges_ASE
  } yield {
    ASEData(e)(t)
    //e._2(t)
  }).toVector

}
