package trackingdataanalysis.pedtrack

import java.io.BufferedReader

import hubmodel.Position
import trackingdataanalysis.{SBBFormat, TrackingDataFormat, VisioSafeNumericFormat}

trait TrajectoryProcessing {


  /** Builds the [[hubmodel.ped.Pedestrian]] population from the trajectory data. The format is defined by the format
    * argument.
    *
    * @param format specification of the file format, include file name
    * @return collection of pedestrian trajectories
    */
  def buildPedestrianTrajectories(format: TrackingDataFormat): Map[Int, hubmodel.ped.Pedestrian] = {
    val population: collection.mutable.Map[Int, hubmodel.ped.Pedestrian] = collection.mutable.Map()
    val bufferedSource: scala.io.BufferedSource = scala.io.Source.fromFile(format.file)
    format match {
      case vs: VisioSafeNumericFormat => {
        for (l <- if (format.headers.isDefined) {bufferedSource.getLines.drop(1)} else {bufferedSource.getLines()}) {
          val cols = l.split(vs.separator).map(str => str.toDouble)
          if (population.contains(cols.last.toInt)) {
            population(cols.last.toInt).updatePositionHistory(VisioSafeNumericFormat.time2Seconds(cols), new Position(cols(8).toDouble / 1000.0, cols(9).toDouble / 1000.0))
          }
          else {
            population += (cols.last.toInt -> new hubmodel.ped.Pedestrian(cols.last.toInt, VisioSafeNumericFormat.time2Seconds(cols)))
            population(cols.last.toInt).updatePositionHistory(VisioSafeNumericFormat.time2Seconds(cols), new Position(cols(8).toDouble / 1000.0, cols(9).toDouble / 1000.0))
          }
        }
      }
      case train: SBBFormat => {
        for (l <- if (format.headers.isDefined) {bufferedSource.getLines.drop(1)} else {bufferedSource.getLines()}) {
          val cols = l.split(train.separator).map(str => str.toDouble)
          if (population.contains(cols(1).toInt)) {
            population(cols(1).toInt).updatePositionHistory(SBBFormat.timeConverter(cols(0)), new Position(cols(2).toDouble / 1000.0, cols(3).toDouble / 1000.0), cols.last.toInt)
          }
          else {
            population += (cols(1).toInt -> new hubmodel.ped.Pedestrian(cols(1).toInt, SBBFormat.timeConverter(cols(0))))
            population(cols(1).toInt).updatePositionHistory(SBBFormat.timeConverter(cols(0)), new Position(cols(2).toDouble / 1000.0, cols(3).toDouble / 1000.0), cols.last.toInt)
          }
        }
      }
    }
    population.toMap
  }


  /** Extracts the extension and file name. The file name can contain multiple dots (although not recommended in general).
    * An Option[(String, String)] is returned.
    *
    * @param fileName file name to split into extension and name
    * @return Optional tuple of strings with the filename and extension
    */
  def extractFileExtension(fileName: String): Option[(String, String)] = {

    // split the file based on '.'
    fileName.split('.').reverse.toList match {
      case ext :: f if f.length == 1 => Some((f.head, ext)) // only extension and file name
      case ext :: f if f.length > 1 => Some((f.mkString(".") , ext)) // file name contains '.' => keep extension and rebuild file name
      case _ => None
    }
  }

  /** Make and educated guess if the first line of the file is a header. If all fields of the first line can be
    * converted to a double, then likely that the first line is a header. This is not bullet proof !
    *
    *
    * Implemented by checking if one of the fields of the first line cannot be converted to double, if yes, then assuming
    * it is a header.
    *
    * @param sep separator to use for splitting the line
    * @param line line to check for headers
    * @return true if first line is header
    */
  def containsHeader(sep: Char)(line: String): Option[Vector[String]] = {

    // convert each column to double and see if all exist
    if (line.split(sep).forall(_.toDoubleOption.isDefined)) {
      None
    } else {
      Some(line.split(sep).toVector)
    }
  }

  /** Check whether the line is split into at least 4 elements. If not, then separator is wrong or there aren't
    * enough columns for this file to be tracking data. At least for columns are needed: time, x, y and ID.
    *
    * @param sep separator to try
    * @param line line to use to test
    * @return
    */
  def successfullSeparator(sep: Char)(line: String): Boolean = {
    if (line.split(sep).length < 4) {false} else {true}
  }

  /** Guesses the format of the data file which is to be analysed. The key assumptions are
    *  - SBB format has 6 columns only numeric separated by commas.
    *  - VisioSafe format has 11 numeric columns columns separated by a commas.
    *
    * @param fileName file to process
    * @return [[TrackingDataFormat]] definition of the format to use to split the data
    */
  def guessDataFormat(fileName: String): TrackingDataFormat = {

    // get file extension
    val (file, extension): (String, String) = extractFileExtension(fileName).get

    // open file for reading
    val fileSource: BufferedReader = scala.io.Source.fromFile(fileName).bufferedReader()

    if (extension == "csv") { // processing of csv files

      // get first line
      val firsLine: String = fileSource.readLine()
      val secondLine: String = fileSource.readLine()


      // possible separators: should be ',' but other are also possible: ';' or '\t'
      val possibleSep: Vector[Char] =  Vector(',', ';', '\t').filter(sep => successfullSeparator(sep)(firsLine))

      // Extracts the only possible separator, otherwise throws an exception
      val sep: Char = if (possibleSep.size > 1) {
        throw new UnsupportedOperationException("Multiple successfull separators, don't know how to choose ! sep=" + possibleSep)
      } else {
        possibleSep.head
      }

      // check if first line is header
      val headers: Option[Vector[String]] = containsHeader(sep)(firsLine)

      // if headers exist, then the first data line is the second line from the file.
      val firstDataLine: String = headers match {
        case Some(_) => { fileSource.readLine() }
        case None => {firsLine}
      }

      firstDataLine.split(sep).toVector match {
        case sbb if sbb.size == 6 && sbb.forall(_.toDoubleOption.isDefined)=> {new SBBFormat(fileName, headers)} // 6 numeric columns
        case vs if vs.size == 11 && vs.forall(_.toDoubleOption.isDefined)=> {new VisioSafeNumericFormat(fileName, headers)} // 11 numeric columns
      }

    } else if (extension == "json") { // processing of JSON files
      // TODO implement the processing of tracking data in JSON format
      ???
    } else {
      throw new UnsupportedOperationException("Cannot parse file with unknown extension ! extension=" + extension)
    }
  }


}
