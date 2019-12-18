package hubmodel

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import hubmodel.DES.NOMADGraphSimulator
import hubmodel.io.input.JSONReaders.PedestrianResults_JSON
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}
import hubmodel.results.ResultsContainerReadNew
import tools.cells.{Rectangle, Vertex}
import myscala.math.stats.{ComputeStats, Statistics}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.BufferedSource
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

package object results {

  /** Used to extract the desired results from the simulator. Avoids keeping all information in memory.
    *
    * The current results are:
    *  - population which completed their trips
    *  - density in zones
    *  - inflow in zones
    *
    * @param simulator simulator from which to extract the results
    * @return results from the simulation
    */
  def collectResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T]): ResultsContainerFromSimulation = {
    if (simulator.exitCode == 0) {
      ResultsContainerFromSimulation(
        simulator.exitCode,
        simulator.populationCompleted,
        simulator.population.toVector,
        simulator.criticalAreas
      )
    } else {
      ResultsContainerFromSimulation(simulator.exitCode, Vector(), Vector(), Map())
    }
  }

  /**
    * Writes the main results from a simulation to csv files. This is needed when running dozens of simulations as the
    * RAM fills up too quickly otherwise.
    *
    * @param simulator simulator with completed results
    * @param prefix    prefix to the file name
    * @param path      path where to write the file, default is empty
    */
  def writeResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T],
                                         prefix: String = "",
                                         dir: String,
                                         writeTrajectoriesVS: Boolean = false,
                                         writeTrajectoriesJSON: Boolean = false,
                                         writeTransfers: Boolean = false
                                        ): Unit = {

    // TODO: check if files exists and remove them if they are inside tmp, and warn about them if they are in output_dir
    if (!Files.exists(Paths.get(dir))) {
      Files.createDirectories(Paths.get(dir))
    }
    /* else {
          Files.newDirectoryStream(Paths.get(dir)).toVector.foreach(f => Files.delete(f))
        }*/
    val path: String = dir /* match {
      case Some(str) => str
      case None => {
        val dirName: String = "tmp-" + simulator.ID + "/"
        if (!Files.exists(Paths.get(dirName))) {
          Files.createDirectory(Paths.get(dirName))
        } else {
          Files.newDirectoryStream(Paths.get(dirName)).toVector.foreach(f => Files.delete(f))
        }
        dirName
      }
    }*/

    // Writes the results to files based on the config file.
    if (simulator.exitCode == 0) { // only write results if simulation completed successfully.

      // write the travel time to a JSON file
        val file = new File(path + prefix + "tt_" + simulator.ID + ".json")
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write("[")
        simulator.population.foreach(p => bw.write(p.toJSON(false) + ",\n"))
        if (simulator.populationCompleted.size == 1) {
          bw.write(simulator.populationCompleted.head.toJSON(true))
        }
        else if (simulator.populationCompleted.size >= 2) {
          simulator.populationCompleted.tail.foreach(p => bw.write(p.toJSON(true) + ",\n"))
          bw.write(simulator.populationCompleted.head.toJSON(true))
        }
        bw.write("]")
        bw.close()

      // write the density measurements to JSON file
      if (simulator.criticalAreas.nonEmpty) {
        val file = new File(path + prefix + "density_" + simulator.ID + ".json")
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write("[")
        simulator.criticalAreas.foreach(ca => bw.write(ca._2.toJSON + ",\n"))
        bw.write("]")
        bw.close()
      }

        (
          simulator.population
            .map(p => (p.origin.name, "", p.travelTime.value, p.entryTime.value, Double.NaN, p.travelDistance)).toSeq
            ++
            simulator.populationCompleted
              .map(p => (p.origin.name, p.finalDestination.name, p.travelTime.value, p.entryTime.value, p.exitTime.value, p.travelDistance))
          )
          .writeToCSV(prefix + "tt_" + simulator.ID + ".csv", columnNames = Some(Vector("origin", "destination", "travelTime", "entryTime", "exitTime", "travelDistance")), rowNames = None, path = path)
      }

    // write only the transferring pedestrians to file.
    if (writeTransfers) {
      simulator.populationCompleted
        .filter(p => simulator.transferringPassengers.contains(p.ID))
        .map(p => (p.origin.name, p.finalDestination.name, p.travelTime.value, p.entryTime.value, p.exitTime.value, p.travelDistance))
        .writeToCSV(prefix + "tt_transfers_" + simulator.ID + ".csv", columnNames = Some(Vector("origin", "destination", "travelTime", "entryTime", "exitTime", "travelDistance")), rowNames = None, path = path)
    }

      // Writes the data for the defined measurement areas.
      Try(
        if (simulator.criticalAreas.nonEmpty) {
          (simulator.criticalAreas.head._2.densityHistory.map(_._1.value).toVector +: simulator.criticalAreas.map(_._2.densityHistory.map(_._2).toVector).toVector).writeToCSV(prefix + "density_" + simulator.ID + ".csv", path)
          simulator.criticalAreas.head._2.paxIndividualDensityHistory.flatMap(v => Vector.fill(v._2.size)(v._1.value).zip(v._2)).toVector.writeToCSV(prefix + "individual_densities_" + simulator.ID + ".csv", path)
        }
      ) match {
        case Success(s) => {
          println("written critical zone successfully")
        }
        case Failure(f) => {
          println("failed writing criticsl zones")
          throw f
        }
    }

    if (writeTrajectoriesVS) {
      println("Writing trajectories as VS to file")
      writePopulationTrajectories(simulator.populationCompleted ++ simulator.population, prefix + "_simulation_trajectories_VS_" + simulator.ID + ".csv")
    }

    if (writeTrajectoriesJSON) {
      println("Writing Trajectories as JSON to file for viz")
      writePopulationTrajectoriesJSON(simulator.populationCompleted ++ simulator.population, prefix + "_simulation_trajectories_" + simulator.ID + ".json", (simulator.startTime.value) to (simulator.finalTime.value) by (simulator.sf_dt.value))
      writeODJSON(simulator.populationCompleted ++ simulator.population, simulator.ODZones.map(_.name), prefix + "_ped_IDS_per_OD_" + simulator.ID + ".json")
    }
  }





  def readResults(dir: String, prefix: String, demandSets: Seq[String]): Iterable[ResultsContainerReadWithDemandSet] = {
    if (demandSets.isEmpty) {
      val dirContents = Files.newDirectoryStream(Paths.get(dir))
      val subDirs: Iterable[String] = dirContents.asScala.toVector.collect({ case f if Files.isDirectory(f) => f.getFileName.toString })
      dirContents.close()
      subDirs.flatMap(ff => readResults(dir + ff, prefix).map(_.addDemandFile(ff)))
    } else {
      demandSets.flatMap(ff => readResults(dir + ff, prefix).map(_.addDemandFile(ff)))
    }
  }


  /**
    * Reads the files located in the argument and processes them to an  Iterable of [[ResultsContainerRead]] object.
    *
    * @param path dir where the files are located
    * @return Iterable containing the results
    */
  def readResults(dir: String, prefix: String): Iterable[ResultsContainerRead] = {
    val path: String = dir
    /*match {
         case Some(str) => str
         case None => "tmp/"
       }*/
    val outputDir = new File(path)
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + path)
    }

    // reads the files populates a map based on the keyword present in the name
    val files: Map[String, Map[String, File]] = outputDir.listFiles.filter(f => f.isFile && f.getName.contains(prefix) && f.getName.endsWith(".csv")).toList.groupBy(f => f.getName.substring(f.getName.indexOf(".csv") - 10, f.getName.indexOf(".csv"))).map(kv => kv._1 -> kv._2.map(f => {
      f.getName match {
        case a if a.contains("_tt_") => "tt"
        case b if b.contains("_density_") => "density"
        case c if c.contains("_individual_densities_") => "individual_densities"
        case d if d.contains("_tt_transfers_") => "tt_transfers"
        case other => throw new IllegalArgumentException("File should not be present: " + other)
      }
    } -> f
    ).toMap)

    files.map(sr => {

      // process travel times file
      val tt: Vector[(String, String, Double, Double, Double, Double)] = {
        val in = scala.io.Source.fromFile(sr._2("tt"))
        val data = (for (line <- in.getLines.drop(1)) yield {
          val cols = line.split(",").map(_.trim)
          if (cols.length == 6) {
            (cols(0), cols(1), cols(2).toDouble, cols(3).toDouble, cols(4).toDouble, cols(5).toDouble)
          } else if (cols.length == 5) {
            (cols(0), cols(1), cols(2).toDouble, cols(3).toDouble, cols(4).toDouble, Double.NaN)
          } else {
            throw new IllegalAccessError("Data does not have the right shape")
          }
        }).toVector
        in.close
        data
      }

      // process density file
      val density: Option[(Vector[Double], Vector[Vector[Double]])] =
        if (sr._2.keySet.contains("density")) {
          val in = scala.io.Source.fromFile(sr._2("density"))
          val data = (for (line <- in.getLines) yield {
            line.split(",").map(_.trim.toDouble)
          }).toVector
          in.close
          Some((data.map(_ (0)), data.map(a => a.tail.toVector))) //data.map(_.toVector).toVector
        } else {
          None
        }


      // process individual density measurements
      val densityPerIndividual: Option[Vector[(BigDecimal, BigDecimal)]] =
        if (sr._2.keySet.contains("individual_densities")) {
          val in = scala.io.Source.fromFile(sr._2("individual_densities"))
          val data = (for (line <- in.getLines if !line.contains("Infinity")) yield {
            Try(line.split(",").map(v => BigDecimal(v.trim))) match {
              case Success(s) => (s(0), s(1));
              case Failure(f) => throw new IllegalArgumentException("error parsing string to BigDecimal: " + line.split(",").map(v => v.trim).mkString(",") + ", " + sr._2("individual_densities") + ", " + f)
            }
          }).toVector
          in.close
          Some(data)
        } else {
          None
        }



      new ResultsContainerRead(tt, density, densityPerIndividual)
    })
  }

  def readResultsJson(dir: String, prefix: String, demandSets: Seq[String]): Iterable[ResultsContainerReadWithDemandSetNew] = {
    if (demandSets.isEmpty) {
      val dirContents = Files.newDirectoryStream(Paths.get(dir))
      val subDirs: Iterable[String] = dirContents.asScala.toVector.collect({ case f if Files.isDirectory(f) => f.getFileName.toString })
      dirContents.close()
      subDirs.flatMap(ff => readResultsJson(dir + ff, prefix).map(_.addDemandFile(ff)))
    } else {
      demandSets.flatMap(ff => readResultsJson(dir + ff, prefix).map(_.addDemandFile(ff)))
    }
  }

  def readResultsJson(dir: String, prefix: String): Iterable[ResultsContainerReadNew] = {
    val path: String = dir
    /*match {
         case Some(str) => str
         case None => "tmp/"
       }*/
    val outputDir = new File(path)
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + path)
    }

    val filesJson: Map[String, Map[String, File]] = outputDir
      .listFiles
      .filter(f => f.isFile && f.getName.contains(prefix) && f.getName.endsWith(".json"))
      .toList.groupBy(f => f.getName.substring(f.getName.indexOf(".json") - 10, f.getName.indexOf(".json")))
      .map(kv => kv._1 -> kv._2.map(f => {
        f.getName match {
          case a if a.contains("_tt_") => "tt"
          case b if b.contains("_density_") => "density"
          case c if c.contains("_individual_densities_") => "individual_densities"
          case other => throw new IllegalArgumentException("File should not be present: " + other)
        }
      } -> f
      ).toMap
      )

    filesJson.map(str => {

      val tt: Vector[PedestrianResults_JSON] = {
        val source: BufferedSource = scala.io.Source.fromFile(str._2("tt"))
        val input: JsValue = Json.parse(try source.mkString finally source.close)
        input.validate[Vector[PedestrianResults_JSON]] match {
          case s: JsSuccess[Vector[PedestrianResults_JSON]] => {
            s.get
          }
          case e: JsError => throw new Error("Error while parsing results file: " + str + "\nerror: " + JsError.toJson(e).toString())
        }
      }

      // TODO implement reading density results using JSON

      new ResultsContainerReadNew(tt, None, None)
    })
  }

  /**
    * Implicit class for processing the pedestrian results. This means the anaylsis can be chained without having
    * to pass the population as an argument. This class should be used directly on the results from the
    * simulation.
    *
    * @param pop collection of [[PedestrianSim]] to analyse
    */
  implicit class PopulationProcessing(pop: Iterable[PedestrianSim]) {

    /** Based on the functions passed as argument, computes the metrics per time window of all the pedestrians satisfying
      * the predicate "filter".
      *
      * @param filter     predicate used to filter the population
      * @param pedFunc    extracts the metric from an individual
      * @param windowFunc computes the index of the window in which the pedestrian belongs
      * @return map where the keys are the time intervals and the values the statstics of the metric
      */
    def aggregateMetricByTimeWindow(filter: PedestrianSim => Boolean, pedFunc: PedestrianSim => Double, windowFunc: PedestrianSim => Double): Map[Double, Statistics[Double]] = {
      this.pop.filter(filter).groupBy(windowFunc).map(grouped => grouped._1 -> grouped._2.map(pedFunc).statistics)
    }

    /**
      * Groups the pedestrians by OD and then computes the statistics of the KPI extraced using the pedFunc argument.
      *
      * @param filter  removes undesired pedestrians
      * @param pedFunc extracts the KPI from the pedestrian
      * @return [[Statistics]] of the KPI from the pedestrians
      */
    def aggregateMetricByOD(filter: PedestrianSim => Boolean, pedFunc: PedestrianSim => Double): Map[(Vertex, Vertex), Statistics[Double]] = {
      this.pop.filter(filter).groupBy(p => (p.origin, p.finalDestination)).map(grouped => grouped._1 -> grouped._2.map(pedFunc).statistics)
    }
  }

  /**
    * Implicit class for processing the pedestrian results. This means the anaylsis can be chained without having
    * to pass the population as an argument. This class shoulc be used on the travel time summary read from the
    * intermediate results.
    *
    * @param popSummary summary of the pedestrian's travel times to analyse
    */
  implicit class PopulationSummaryProcessing(popSummary: Vector[(String, String, Double, Double, Double)]) {

    /** Based on the functions passed as argument, computes the metrics per time window of all the pedestrians satisfying
      * the predicate "filter".
      *
      * @param f          predicate used to filter the population
      * @param pedFunc    extracts the metric from an individual
      * @param windowFunc computes the index of the window in which the pedestrian belongs
      * @return map where the keys are the time intervals and the values the statstics of the metric
      */
    def aggregateMetricByTimeWindow(f: Tuple5[String, String, Double, Double, Double] => Boolean, pedFunc: Tuple5[String, String, Double, Double, Double] => Double, windowFunc: Tuple5[String, String, Double, Double, Double] => Double): Map[Double, Statistics[Double]] = {
      this.popSummary.filter(d => f(d)).groupBy(d => windowFunc(d)).map(grouped => grouped._1 -> grouped._2.map(d => pedFunc(d)).statistics)
    }

    /**
      * Groups the pedestrians by OD and then computes the statistics of the KPI extraced using the pedFunc argument.
      *
      * @param f       removes undesired pedestrians
      * @param pedFunc extracts the KPI from the pedestrian summary
      * @return [[Statistics]] of the KPI from the pedestrians
      */
    def aggregateMetricByOD(f: Tuple5[String, String, Double, Double, Double] => Boolean, pedFunc: Tuple5[String, String, Double, Double, Double] => Double): Map[(String, String), Statistics[Double]] = {
      this.popSummary.filter(d => f(d)).groupBy(p => (p._1, p._2)).map(grouped => grouped._1 -> grouped._2.map(d => pedFunc(d)).statistics)
    }
  }

}
