package optimization

import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter

object ParetoFromCSV extends App {

  val idx1: Int = 5
  val idx2: Int = 6
  val fileName: String = "/home/nicholas/PhD/code/hub-simulator/SO_gating_KPIs_bHbYunNRq.csv"


  val bufferedSourceHeader = io.Source.fromFile(fileName)
  val headers: Vector[String] = (for (line <- bufferedSourceHeader.getLines.take(1)) yield {
    line.split(",").map(_.trim).toVector
  }).toVector.head


  val bufferedSource = io.Source.fromFile(fileName)
  val data: Seq[Seq[Double]] = (for (line <- bufferedSource.getLines.drop(1)) yield {
    line.split(",").map(_.trim.toDouble).toSeq
  }).toSeq

  val min1: (Double, Int) = data.map(_ (idx1)).zipWithIndex.minBy(_._1)
  val min2: (Double, Int) = data.map(_ (idx2)).zipWithIndex.minBy(_._1)


  val sortedIdx1: Seq[Seq[Double]] = data.filter(v => v(idx1) <= data(min2._2)(idx1) && v(idx2) <= data(min1._2)(idx2)).sortBy(_ (idx1))
  val size: Int = sortedIdx1.size

  var previousPointIdx: Int = 0
  val pareto: Seq[Seq[Double]] = sortedIdx1.head +: (for (i <- 1 until size if sortedIdx1(previousPointIdx)(idx2) > sortedIdx1(i)(idx2)) yield {
    previousPointIdx = i
    sortedIdx1(i)
  }).toVector

  pareto.transpose.writeToCSV(fileName.replace(".csv", "_pareto.csv"), rowNames = None, columnNames = Some(headers))

  bufferedSourceHeader.close
  bufferedSource.close

}
