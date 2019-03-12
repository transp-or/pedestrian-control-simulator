package hubmodel

import java.io.File
import java.nio.file.{Files, Paths}

import hubmodel.DES.NOMADGraphSimulator
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}
import hubmodel.tools.cells.Rectangle
import myscala.math.stats.{ComputeStats, Statistics}

import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
import scala.util.{Failure, Success, Try}

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
  def writeResults[T <: PedestrianNOMAD](simulator: NOMADGraphSimulator[T], prefix: String = "", dir: String, writeTrajectoriesVS: Boolean = false, writeTrajectoriesJSON: Boolean = false, writeTRANSFORMTT: Boolean = false): Unit = {

    // TODO: check if files exists and remove them if they are inside tmp, and warn about them if they are in output_dir
    if (!Files.exists(Paths.get(dir))) {
      Files.createDirectory(Paths.get(dir))
    }/* else {
      Files.newDirectoryStream(Paths.get(dir)).toVector.foreach(f => Files.delete(f))
    }*/
    val path: String = dir/* match {
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

    if (simulator.exitCode == 0) {
      if (writeTRANSFORMTT) {
        simulator.populationCompleted
          .filter(p => simulator.transferringPassengers.contains(p.ID))
          .map(p => (p.origin.name, p.finalDestination.name, p.travelTime.value, p.entryTime.value, p.exitTime.value, p.travelDistance))
          .writeToCSV(prefix + "tt_" + simulator.ID + ".csv", columnNames=Some(Vector("origin", "destination", "travelTime", "entryTime", "exitTime", "travelDistance")), rowNames=None, path=path)

      } else {
        simulator.populationCompleted
          .map(p => (p.origin.name, p.finalDestination.name, p.travelTime.value, p.entryTime.value, p.exitTime.value, p.travelDistance))
          .writeToCSV(prefix + "tt_" + simulator.ID + ".csv", columnNames=Some(Vector("origin", "destination", "travelTime", "entryTime", "exitTime", "travelDistance")), rowNames=None, path=path)

      }
      if (simulator.criticalAreas.nonEmpty) {
        (simulator.criticalAreas.head._2.densityHistory.map(_._1.value).toVector +: simulator.criticalAreas.map(_._2.densityHistory.map(_._2).toVector).toVector).writeToCSV(prefix + "density_" + simulator.ID + ".csv", path)
        simulator.criticalAreas.head._2.paxIndividualDensityHistory.flatMap(v => Vector.fill(v._2.size)(v._1.value).zip(v._2)).toVector.writeToCSV(prefix + "individual_densities_" + simulator.ID + ".csv", path)
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
    demandSets.flatMap(ff => readResults(dir + ff, prefix).map(_.addDemandFile(ff)))
  }


    /**
    * Reads the files located in the argument and processes them to an  Iterable of [[ResultsContainerRead]] object.
    *
    * @param path dir where the files are located
    * @return Iterable containing the results
    */
  def readResults(dir: String, prefix: String): Iterable[ResultsContainerRead] = {
    val path: String = dir /*match {
      case Some(str) => str
      case None => "tmp/"
    }*/
    val outputDir = new File(path)
    if (!outputDir.exists || !outputDir.isDirectory) {
      throw new IllegalArgumentException("Output dir for files does not exist ! dir=" + path)
    }

    // reads the files populates a map based on the keyword present in the name
    val files: Map[String, Map[String, File]] = outputDir.listFiles.filter(f => f.isFile && f.getName.contains(prefix)).toList.groupBy(f => f.getName.substring(f.getName.indexOf(".csv") - 10, f.getName.indexOf(".csv"))).map(kv => kv._1 -> kv._2.map(f => {
      f.getName match {
        case a if a.contains("_tt_") => "tt"
        case b if b.contains("_density_") => "density"
        case c if c.contains("_individual_densities_") => "individual_densities"
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
          (cols(0), cols(1), cols(2).toDouble, cols(3).toDouble, cols(4).toDouble, cols(5).toDouble)
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

      ResultsContainerRead(tt, density, densityPerIndividual)
    })
  }

  /**
    * Implicit class for processing the pedestrian results. This means the anaylsis can be chained without having
    * to pass the population as an argument. This class should be used directly on the results from the
    * simulation.
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
      * @param filter removes undesired pedestrians
      * @param pedFunc extracts the KPI from the pedestrian
      * @return [[Statistics]] of the KPI from the pedestrians
      */
    def aggregateMetricByOD(filter: PedestrianSim => Boolean, pedFunc: PedestrianSim => Double): Map[(Rectangle, Rectangle), Statistics[Double]] = {
      this.pop.filter(filter).groupBy(p => (p.origin, p.finalDestination)).map(grouped => grouped._1 -> grouped._2.map(pedFunc).statistics)
    }
  }

  /**
    * Implicit class for processing the pedestrian results. This means the anaylsis can be chained without having
    * to pass the population as an argument. This class shoulc be used on the travel time summary read from the
    * intermediate results.
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
      * @param f removes undesired pedestrians
      * @param pedFunc extracts the KPI from the pedestrian summary
      * @return [[Statistics]] of the KPI from the pedestrians
      */
    def aggregateMetricByOD(f: Tuple5[String, String, Double, Double, Double] => Boolean, pedFunc: Tuple5[String, String, Double, Double, Double] => Double): Map[(String, String), Statistics[Double]] = {
      this.popSummary.filter(d => f(d)).groupBy(p => (p._1, p._2)).map(grouped => grouped._1 -> grouped._2.map(d => pedFunc(d)).statistics)
    }
  }

}
