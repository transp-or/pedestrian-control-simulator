import java.io.{BufferedWriter, FileWriter}

import breeze.numerics.pow
import trackingdataanalysis.pedtrack.NewBetterPosition2D
import trackingdataanalysis.pedtrack.georeferencing.linearTransformation

object DistributeFlowsInNewLausanneStation extends App {

  // WORL MERCATOR mapping parameters for PIW
  /*val mappingFunction: NewBetterPosition2D => NewBetterPosition2D = linearTransformation(
    3.308796780805034*pow(10.0,-4),
    1.471699610853366*pow(10.0,-3),
    7.378216680910615*pow(10.0,5),
    1.510743808557866*pow(10.0,-3),
    -2.453869596074234*pow(10.0,-4),
    5.832467066569241*pow(10.0,6) )*/

  val extension: String = ".csv"
  val path: String = "/home/nicholas/visiosafe-data/lausanne-tracking-data/"
  val fileNamesPIW: Vector[String] = Vector(
    "lausanne_2013_01_22_piw",
    "lausanne_2013_01_23_piw",
    "lausanne_2013_02_06_piw",
    "lausanne_2013_02_27_piw",
    "lausanne_2013_02_28_piw",
    "lausanne_2013_03_05_piw",
    "lausanne_2013_04_09_piw",
    "lausanne_2013_04_10_piw",
    "lausanne_2013_04_18_piw",
    "lausanne_2013_04_30_piw",
  )
  val fileNamesPIE: Vector[String] = Vector(
    "lausanne_2013_01_22_pie",
    "lausanne_2013_01_23_pie",
    "lausanne_2013_02_06_pie",
    "lausanne_2013_02_27_pie",
    "lausanne_2013_02_28_pie",
    "lausanne_2013_03_05_pie",
    "lausanne_2013_04_09_pie",
    "lausanne_2013_04_10_pie",
    "lausanne_2013_04_18_pie",
    "lausanne_2013_04_30_pie",
  )

  // UTM 32N WGS 84 (EPSG:32632) mapping for PIW
  val mappingFunctionPIW: NewBetterPosition2D => NewBetterPosition2D = linearTransformation(
    2.592811416037474*pow(10.0,-4),
    1.008975307217177*pow(10.0,-3),
    3.180464996409054*pow(10.0,5),
    1.034131970779907*pow(10.0,-3),
    -1.995806279935558*pow(10.0,-4),
    5.154153277336739*pow(10.0,6) )

  for (f <- fileNamesPIW) {

    // create files to read and write
    val inputFile = path + f + extension
    val outputFile = path + f + "_UTM32N-WGS84.dat"

    // opens files
    val bufferedSource: scala.io.BufferedSource = io.Source.fromFile(inputFile)
    val bw = new BufferedWriter(new FileWriter(outputFile))

    // iterates through each line of the file and applies the conversion to the xy coordinates
    for (l <- bufferedSource.getLines.drop(1)) {
      val cols = l.split(",")
      val posG: NewBetterPosition2D = mappingFunctionPIW(new NewBetterPosition2D(cols(8).toDouble, cols(9).toDouble))
      bw.write(cols(0) + "-" + f"${cols(1).toInt}%02d" + "-" + f"${cols(2).toInt}%02d" + "T" + f"${cols(3).toInt}%02d" + ":" + f"${cols(4).toInt}%02d" + ":" + f"${cols(5).toInt}%02d" + "." + f"${cols(6).toInt}%03d" + ";" + cols(7) + ";" + posG.X + ";" + posG.Y + ";" + cols(10) + "\n")
    }

    // closes both files
    bufferedSource.close()
    bw.close()
  }


  // UTM 32N WGS 84 (EPSG:32632) mapping for PIE
  val mappingFunctionPIE: NewBetterPosition2D => NewBetterPosition2D = linearTransformation(
    2.415533110046536*pow(10.0,-4),
    9.877355424660701*pow(10.0,-4),
    3.181626047096282*pow(10.0,5),
    9.853920924180611*pow(10.0,-4),
    -2.368806151087312*pow(10.0,-4),
    5.154148566965720*pow(10.0,6) )

  for (f <- fileNamesPIE) {

    // create files to read and write
    val inputFile = path + f + extension
    val outputFile = path + f + "_UTM32N-WGS84.dat"

    // opens files
    val bufferedSource: scala.io.BufferedSource = io.Source.fromFile(inputFile)
    val bw = new BufferedWriter(new FileWriter(outputFile))

    // iterates through each line of the file and applies the conversion to the xy coordinates
    for (l <- bufferedSource.getLines.drop(1)) {
      val cols = l.split(",")
      val posG: NewBetterPosition2D = mappingFunctionPIE(new NewBetterPosition2D(cols(8).toDouble, cols(9).toDouble))
      bw.write(cols(0) + "-" + f"${cols(1).toInt}%02d" + "-" + f"${cols(2).toInt}%02d" + "T" + f"${cols(3).toInt}%02d" + ":" + f"${cols(4).toInt}%02d" + ":" + f"${cols(5).toInt}%02d" + "." + f"${cols(6).toInt}%03d" + ";" + cols(7) + ";" + posG.X + ";" + posG.Y + ";" + cols(10) + "\n")
    }

    // closes both files
    bufferedSource.close()
    bw.close()
  }


  val inputFileZones: String = "/home/nicholas/visiosafe-data/lausanne-metadata/ODzonesUnderpassesLausanne_Coordinates_Corrected.csv"
  val outputFileZones: String = "/home/nicholas/visiosafe-data/lausanne-metadata/ODzonesUnderpassesLausanne_Coordinates_Corrected_UTM32N-WGS84.dat"


  val bufferedSourceZones: scala.io.BufferedSource = io.Source.fromFile(inputFileZones)
  val bwZones = new BufferedWriter(new FileWriter(outputFileZones))
  for (l <- bufferedSourceZones.getLines.drop(1)) {
    val cols = l.split(";")
    val mappingFunction = if (cols(0).toInt > 0 && cols(0).toInt <= 14) { mappingFunctionPIW }
    else if (cols(0).toInt >= 15 && cols(0).toInt < 24) { mappingFunctionPIE }
    else { throw new RuntimeException }
    val posG1: NewBetterPosition2D = mappingFunction(new NewBetterPosition2D(cols(2).toDouble, cols(3).toDouble))
    val posG2: NewBetterPosition2D = mappingFunction(new NewBetterPosition2D(cols(4).toDouble, cols(5).toDouble))
    val posG3: NewBetterPosition2D = mappingFunction(new NewBetterPosition2D(cols(6).toDouble, cols(7).toDouble))
    val posG4: NewBetterPosition2D = mappingFunction(new NewBetterPosition2D(cols(8).toDouble, cols(9).toDouble))
    bwZones.write(cols(0) + ";" + cols(1) + ";" + posG1.X + ";" + posG1.Y + ";" + posG2.X + ";" + posG2.Y + ";" + posG3.X + ";" + posG3.Y + ";" + posG4.X + ";" + posG4.Y + "\n")
  }
  bufferedSourceZones.close()
  bwZones.close()
}
