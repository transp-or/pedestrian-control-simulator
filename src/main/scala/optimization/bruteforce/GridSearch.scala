package optimization.bruteforce

import java.io.File

trait GridSearch {

  protected def ProcessTTFile1Parameter(f: File): (Double, Map[(String, String), Vector[Double]]) = {
    val endParams: Int = f.getName.indexOf("_params_tt_")
    val params = f.getName.substring(0, endParams).split("_").map(_.toDouble).toVector
    val in = scala.io.Source.fromFile(f)
    val tt: scala.collection.immutable.SortedMap[(String, String), Vector[Double]] = scala.collection.immutable.SortedMap[(String, String), Vector[Double]]() ++ (for (line <- in.getLines) yield {
      val cols = line.split(",").map(_.trim)
      (cols(0), cols(1), cols(2).toDouble)
    }).toVector.groupBy(tup => (tup._1, tup._2)).mapValues(v => v.map(_._3))
    in.close
    (params(0), tt)
  }

  protected def ProcessTTFile2Parameters(f: File): (Double, Double, Map[(String, String), Vector[Double]]) = {
    val endParams: Int = f.getName.indexOf("_params_tt_")
    val params = f.getName.substring(0, endParams).split("_").map(_.toDouble).toVector
    val in = scala.io.Source.fromFile(f)
    val tt: scala.collection.immutable.SortedMap[(String, String), Vector[Double]] = scala.collection.immutable.SortedMap[(String, String), Vector[Double]]() ++ (for (line <- in.getLines) yield {
      val cols = line.split(",").map(_.trim)
      (cols(0), cols(1), cols(2).toDouble)
    }).toVector.groupBy(tup => (tup._1, tup._2)).mapValues(v => v.map(_._3))
    in.close
    (params(0), params(1), tt)
  }

}
