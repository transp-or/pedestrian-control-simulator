package trackingdataanalysis.pedtrack.visiosafe


import java.io.{BufferedWriter, File, FileWriter}

//import breeze.linalg.{Axis, DenseMatrix, DenseVector}
//import breeze.stats.regression.{LeastSquaresRegressionResult, leastSquares}
import hubmodel.Position
import hubmodel.tools.cells.Rectangle
import kn.uni.voronoitreemap.datastructure.OpenList
import kn.uni.voronoitreemap.diagram.PowerDiagram
import kn.uni.voronoitreemap.j2d.{PolygonSimple, Site}
import myscala.math.stats.ComputeStats
import myscala.math.vector.Vector2D
import myscala.output.SeqExtension._
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import trackingdataanalysis.pedtrack.Pedestrian
import trackingdataanalysis.pedtrack.io.{CONTROLLED, UNCONTROLLED}
import trackingdataanalysis.visualization.{PlotOptions, ScatterPlot3D}

import scala.jdk.CollectionConverters._

/** VisioSafe processor for one single file
  *
  * @param fileName file to process
  * @param zoneFile location of the file specifying the zones
  */
class SingleDayAggregateProcessor(fileName: String,
                                  zoneFile: String,
                                  tolerance: Double) extends DataProcessor(zoneFile, tolerance) {


  private val strings: Iterator[String] = io.Source.fromFile(fileName).getLines()


  /** Writes the population to JSON file. Each pedestrian is written with his ID and the history of the positions
    * and velocities. Using JSON makes any future modification easier.
    *
    */
  /*def writePedestrians2JSON(fileName: String): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Json.prettyPrint(Json.toJson(this.ped.take(5).map(p => Pedestrian_JSON(p._2.ID.toString, p._2.h_t, p._2.h_x, p._2.h_y, p._2.h_v.map(_._1), p._2.h_v.map(_._2))))))
    bw.close()
  }*/

  val ped: PedestrianMap = this.aggregatePedestrians(fileName)
  this.assignZones()


  lazy val pedSimplified: Iterable[(Int, Vector[Double], Vector[Double], Vector[Double])] = ped.map(p => (p._2.ID, p._2.h_t.toVector, p._2.h_x.toVector, p._2.h_y.toVector))

  private val voronoiDataStorage: collection.mutable.Map[(Rectangle, Seq[Double], PolygonSimple), Vector[(Double, Seq[(Position, PolygonSimple, Site)])]] = collection.mutable.Map()

  val timeMapFuncVal: Seq[Double] => Vector[(Double, Iterable[Int])] = times => collectIDByTime(times, ped.map(p => (p._2.ID, p._2.h_t.toVector, p._2.h_x.toVector, p._2.h_y.toVector)), Vector())

  //lazy val voronoiData: Vector[(Double, Seq[(DenseVector[Double], PolygonSimple, Site)])] = computeVoronoiDiagrams(z, times, voronoiBoundary, tol)


  /** assign zones for single days
    *
    */
  protected def assignZones(): Unit = {
    this.ped.transform((_, ped) => assignZonePedestrian(ped))
  }


  /** Computes the voronoi diagrams for the times steps passed as argument.
    *
    * @param z
    * @param times
    * @param voronoiBoundary
    * @return
    */
  private def computeVoronoiDiagrams(z: Rectangle, times: scala.collection.immutable.Seq[Double], voronoiBoundary: PolygonSimple): scala.collection.immutable.Vector[(Double, scala.collection.immutable.Seq[(Position, PolygonSimple, Site)])] = {

    //val pedSimplified: Iterable[(Int, Vector[Double], Vector[Double], Vector[Double])] = ped.map(p => (p._2.ID, p._2.h_t.toVector, p._2.h_x.toVector, p._2.h_y.toVector))
    val timeMap: Vector[(Double, Iterable[Int])] =
      ped.values.flatMap(p => {

        times.collect {
          case vidTime if {
            p.h_t.head <= vidTime && vidTime <= p.h_t.last
          } => {
            val diff2VidTime: Vector[(Double, Int)] = p.h_t.map(sampleTime => math.abs(sampleTime - vidTime)).zipWithIndex.toVector
            val minFirst: (Double, Int) = diff2VidTime.minBy(_._1)
            val minSecond: (Double, Int) = diff2VidTime.filterNot(_._2 == minFirst._2).minBy(_._1)
            if (p.h_t(minFirst._2) < p.h_t(minSecond._2)) {
              (vidTime, p.ID, linearInterpolationPosition((p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), p.h_t(minFirst._2), p.h_t(minSecond._2), vidTime))
            } else {
              (vidTime, p.ID, linearInterpolationPosition((p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), p.h_t(minSecond._2), p.h_t(minFirst._2), vidTime))
            }
          }
        }.filter(tPos => z.isInside(new Position(tPos._3._1, tPos._3._2)))
      }).groupBy(tup => tup._1).map(tup => (tup._1, tup._2.map(_._2))).toVector
    //val timeMap: Vector[(Double, Iterable[Int])] = collectIDByTime(times, this.pedSimplified, Vector())
    val res = timeMap.map(p => {
      if (p._2.nonEmpty) {
        val voronoi: PowerDiagram = new PowerDiagram()
        val stupidList: OpenList = new OpenList()
        p._2.foreach(id => {
          val idx: Int = ped(id).h_t.indexWhere(t => t >= p._1)
          if (idx >= 0 && idx < ped(id).h_t.size - 1) {
            val pos: (Double, Double) = linearInterpolationPosition((ped(id).h_x(idx), ped(id).h_y(idx)), (ped(id).h_x(idx + 1), ped(id).h_y(idx + 1)), ped(id).h_t(idx), ped(id).h_t(idx + 1), p._1)
            if (z.isInside(new Position(pos._1, pos._2))) {
              stupidList.add(new Site(pos._1, pos._2))
            }
          }
          /*ped(id).h_t.indexWhere(t => abs(t - p._1) < tol) match {
            case i if i >= 0 && z.isInside((ped(id).h_x(i), ped(id).h_y(i))) => stupidList.add(new Site(ped(id).h_x(i), ped(id).h_y(i)))
            case err => //System.err.println("Pedestrian outside spatio-temporal zone !")
          }*/
        }) //.foreach(stupidList.add)
        //println(p._1, stupidList.map(s => s.getPoint))
        if (stupidList.size > 0) {
          voronoi.setSites(stupidList)
          voronoi.setClipPoly(voronoiBoundary)
          (p._1, voronoi.computeDiagram().asScala.toVector.filter(s => z.isInside(new Position(s.x, s.y))))
        }
        else {
          (p._1, scala.collection.immutable.Vector())
        }
      }
      else {
        (p._1, scala.collection.immutable.Vector())
      }
    }).map(t => (t._1, t._2.map(s => (new Position(s.x, s.y), s.getPolygon, s))))

    // as time stamps where no pedestrian is inside the zone are removed, these empty cells must be added again.
    (res ++ (for (t <- times if !res.exists(_._1 == t)) yield {
      (t, scala.collection.immutable.Vector())
    }).toVector).sortBy(_._1)
  }


  def getVoronoiData(z: Rectangle, times: Seq[Double], voronoiBoundary: PolygonSimple): Vector[(Double, Seq[(Position, PolygonSimple, Site)])] = {
    this.voronoiDataStorage.getOrElseUpdate((z, times, voronoiBoundary), computeVoronoiDiagrams(z, times, voronoiBoundary))
  }


  /* def computeVoronoiDiagramsToTidy(z: Zone, times: Seq[Double], voronoiBoundary: PolygonSimple, tol: Double = 0.05): Vector[(Double, Seq[Site])] = {

     val pedSimplified: Iterable[(Int, Vector[Double], Vector[Double], Vector[Double])] = ped.map(p => (p._2.ID, p._2.h_t.toVector, p._2.h_x.toVector, p._2.h_y.toVector))
     val timeMap: Vector[(Double, Iterable[Int])] = collectIDByTime(times, pedSimplified, Vector())

     timeMap.map(p => {
       if (p._2.nonEmpty) {
         val voronoi: PowerDiagram = new PowerDiagram()
         val stupidList: OpenList = new OpenList()
         p._2.map(id => {
           ped(id).h_t.indexWhere(t => abs(t - p._1) < tol) match {
             case i if i >= 0 => new Site(ped(id).h_x(i), ped(id).h_y(i))
           }
         }).foreach(stupidList.add)
         voronoi.setSites(stupidList)
         voronoi.setClipPoly(voronoiBoundary)
         (p._1, voronoi.computeDiagram().filter(s => isInRectangle((s.x, s.y), z)))
       }
       else {
         (p._1, Seq())
       }
     })//.map(t => (t._1, t._2.map(s => (DenseVector(s.x, s.y), s.getPolygon))))
   }*/


  //lazy val voronoiData: Vector[(Double, Seq[(DenseVector[Double], PolygonSimple, Site)])] =

  /**
    *
    * @param z
    * @param times
    * @param voronoiBoundary
    * @param tol
    * @return
    */
  def computeVoronoiDensity(z: Rectangle, times: Seq[Double], voronoiBoundary: PolygonSimple): Vector[(Double, Double)] = {
    getVoronoiData(z, times, voronoiBoundary).map(v => (v._1, v._2.map(_._2).foldLeft(0.0) { (n: Double, m: PolygonSimple) => n + 1.0 / (m.getArea * v._2.size) }))
  }


  /** Computes the density by counting the number of people inside the area.
    *
    * @param z     zone in which to count people
    * @param times times at chich to
    * @return
    */
  def computeAccumulationDensity(z: Rectangle, times: Seq[Double]): IndexedSeq[(Double, Double)] = {
    val res = ped.values.flatMap(p => {
      times.collect {
        case vidTime if {
          p.h_t.head <= vidTime && vidTime <= p.h_t.last
        } => {
          val diff2VidTime: Vector[(Double, Int)] = p.h_t.map(sampleTime => scala.math.abs(sampleTime - vidTime)).zipWithIndex.toVector
          val minFirst: (Double, Int) = diff2VidTime.minBy(_._1)
          val minSecond: (Double, Int) = diff2VidTime.filterNot(_._2 == minFirst._2).minBy(_._1)
          if (p.h_t(minFirst._2) < p.h_t(minSecond._2)) {
            println(vidTime, (p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), linearInterpolationPosition((p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), p.h_t(minFirst._2), p.h_t(minSecond._2), vidTime))
            (vidTime, linearInterpolationPosition((p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), p.h_t(minFirst._2), p.h_t(minSecond._2), vidTime))
          } else {
            println(vidTime, (p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), linearInterpolationPosition((p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), p.h_t(minSecond._2), p.h_t(minFirst._2), vidTime))
            (vidTime, linearInterpolationPosition((p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), p.h_t(minSecond._2), p.h_t(minFirst._2), vidTime))
          }
        }
      }.filter(tPos => {
        z.isInside(new Position(tPos._2._1, tPos._2._2))
      })
    }).groupBy(t => t._1).map(d => (d._1, d._2.size / z.area)).toVector

    println(ped.values.flatMap(p => {
      times.collect {
        case vidTime if {
          p.h_t.head <= vidTime && vidTime <= p.h_t.last
        } => {
          val diff2VidTime: Vector[(Double, Int)] = p.h_t.map(sampleTime => scala.math.abs(sampleTime - vidTime)).zipWithIndex.toVector
          val minFirst: (Double, Int) = diff2VidTime.minBy(_._1)
          val minSecond: (Double, Int) = diff2VidTime.filterNot(_._2 == minFirst._2).minBy(_._1)
          if (p.h_t(minFirst._2) < p.h_t(minSecond._2)) {
            println(vidTime, (p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), linearInterpolationPosition((p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), p.h_t(minFirst._2), p.h_t(minSecond._2), vidTime))
            (vidTime, linearInterpolationPosition((p.h_x(minFirst._2), p.h_y(minFirst._2)), (p.h_x(minSecond._2), p.h_y(minSecond._2)), p.h_t(minFirst._2), p.h_t(minSecond._2), vidTime))
          } else {
            println(vidTime, (p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), linearInterpolationPosition((p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), p.h_t(minSecond._2), p.h_t(minFirst._2), vidTime))
            (vidTime, linearInterpolationPosition((p.h_x(minSecond._2), p.h_y(minSecond._2)), (p.h_x(minFirst._2), p.h_y(minFirst._2)), p.h_t(minSecond._2), p.h_t(minFirst._2), vidTime))
          }
        }
      }.filter(tPos => {
        z.isInside(new Position(tPos._2._1, tPos._2._2))
      })
    }).groupBy(t => t._1))
    (res ++ (for (t <- times if !res.exists(_._1 == t)) yield {
      (t, 0.0)
    }).toVector).sortBy(_._1)

    /*times.collect{case t if {val idx: Int = p.h_t.indexWhere(_ > t); idx >= 0 && idx < p.h_t.size-1} => {
      val idx: Int = p.h_t.indexWhere(_ > t)
      (t, linearInterpolationPosition((p.h_x(idx), p.h_y(idx)), (p.h_x(idx+1), p.h_y(idx+1)), p.h_t(idx), p.h_t(idx+1), t))
    }}.filter(tPos => z.isInside(tPos._2))
  }).groupBy(t => t._1).map(d => (d._1, d._2.size/z.area)).toVector*/

    // fill with zeros the missing times.
    //(res ++ (for (t <- times if !res.exists(_._1 == t)) yield {(t, 0.0)}).toVector).sortBy(_._1)
  }

  /**
    *
    * @param file
    * @param path
    */
  def writeDisaggregateODToCSV(file: String = fileName, path: String = ""): Unit = {
    this.ped.map(p => (p._2.UUID, p._2.entryTime, p._2.oZone, p._2.exitTime, p._2.dZone)).toVector.writeToCSV(file, path)
  }


  /*val pedSimplified: Iterable[(Int, Vector[Double], Vector[Double], Vector[Double])] = ped.map(p => (p._2.ID, p._2.h_t.toVector, p._2.h_x.toVector, p._2.h_y.toVector))
  val z: Zone = new Zone(Array(100,0,72000,11000,80000,11000,80000,16000,72000,16000))
  val times = 7.0*3600.0 to 8.0*3600.0 by 5

  val timeMap: Vector[(Double, Iterable[Int])] = collectIDByTime(times, pedSimplified, Vector())*/


  /* def computeInflowOutflow(mainZone: Zone, times: Seq[Double]): Vector[(Double, Double, Int, Int)] = {

     val pedSimplified: Iterable[(Int, Vector[Double], Vector[Double], Vector[Double])] = ped.map(p => (p._2.ID, p._2.h_t.toVector, p._2.h_x.toVector, p._2.h_y.toVector))
     val timeMap: Vector[(Double, Iterable[Int])] = collectIDByTime(times, pedSimplified, Vector())

     def helper(timeMap: Vector[(Double, Iterable[Int])], acc: Vector[(Double, Double, Int, Int)]): Vector[(Double, Double, Int, Int)] = {
       if (timeMap.size == 1) acc
       else {
         // get ids of pedestrian which are in the system at the current time step and the next time step
         val ids: Vector[Int] = (timeMap.head._2 ++ timeMap.tail.head._2).toVector.distinct

         // for each pedestrian, get the row number for the positions at the current and next time steps. At the current
         // time step means within a specific tolerance.
         val entryExitIdxAppear: Vector[(Int, (Int, Int))] = ids.zip(
           ids.map(id => ped(id).h_t.indexWhere(t => abs(t - timeMap.head._1) < 0.05)).zip(
             ids.map(id => ped(id).h_t.indexWhere(t => abs(t - timeMap.tail.head._1) < 0.05))))

         // count number of people who entered the system after the current time and are inside the main zone
         val enteredSystem: Int = entryExitIdxAppear.count(t => t._2._1 == -1 && isInRectangle((ped(t._1).h_x(t._2._2), ped(t._1).h_y(t._2._2)), mainZone))
         // count number of people who exited the system by the next time and are inside the main zone at the current time
         val exitedSystem: Int = entryExitIdxAppear.count(t => t._2._2 == -1 && isInRectangle((ped(t._1).h_x(t._2._1), ped(t._1).h_y(t._2._1)), mainZone))

         // keep only pedestrians who's trajectories cover the full time span.
         val entryExitIdxFullyContained: Vector[(Int, (Int, Int))] = entryExitIdxAppear.filterNot(_._2._2 == -1).filterNot(_._2._1 == -1)

         // count the pedestrians who were outside the main zone at the current time and are inside at the next time
         val entered: Int = entryExitIdxFullyContained.count(l => {
           !isInRectangle((ped(l._1).h_x(l._2._1), ped(l._1).h_y(l._2._1)), mainZone) && isInRectangle((ped(l._1).h_x(l._2._2), ped(l._1).h_y(l._2._2)), mainZone)
         })

         // count the pedestrians who were inside the main zone at the current time and are outside at the next time
         val exited: Int = entryExitIdxFullyContained.count(l => {
           isInRectangle((ped(l._1).h_x(l._2._1), ped(l._1).h_y(l._2._1)), mainZone) && !isInRectangle((ped(l._1).h_x(l._2._2), ped(l._1).h_y(l._2._2)), mainZone)
         })

         helper(timeMap.tail, acc :+ (timeMap.head._1, timeMap.tail.head._1, entered + enteredSystem, exited + exitedSystem))
       }
     }
     helper(timeMap, Vector())
   }*/

  /** Counts the number of people changing zones at each time interval. The goal is to count the number of people
    * entering/exiting the main zone (i.e. zone in which we are interested in analysing the MFD relations) from
    * the adjacent zones. The collection of adjacent zones is the second argument, and represents the controlled flows.
    * Finally, people who enter/exit the main zone from other places go into zones -1 and represent
    * the non controlled flows.
    *
    * @param mainZone zone in which the fundamental relations are considered
    * @param zones    the collection of controlled entry/exit links
    * @param times    the time intervals to use for aggregation
    * @return Map where the keys are (time interval index, origin zone, destination zone) and the values the number of people
    */
  def computeFlowsNew(mainZone: Rectangle, zones: Vector[Rectangle], times: Vector[Double]): Map[(Int, Int, Int), Int] = {
    val numberTimeIntervals = times.size

    /** Finds the interval corresponding to a specific time in a recursive way.
      * For a specific time t, this will return the index i such that t is in [ times(i), times(i+1) )
      * If t is smaller than times.head, returns -1, if t is larger than times.last, returns -2.
      *
      * @param t time to search location for
      * @return intevral in which the time is located
      *
      */
    def findTimesIndex(t: Double) = times.indexWhere(_ < t) - 1

    def findTimesIdx(t: Double): Int = {

      def helper(currentIdx: Int): Int = {
        if (currentIdx == numberTimeIntervals - 1) -2
        else if (times(currentIdx) <= t && t < times(currentIdx + 1)) currentIdx
        else helper(currentIdx + 1)
      }

      if (t < times.head) -1
      else helper(0)
    }

    /** Finds the zone in which a specific point is located.
      *
      * @param x x coordinate
      * @param y y coordinate
      * @return id of the zone, -1 if none are found.
      */
    def findZone(x: Double, y: Double): Int = {
      (mainZone +: zones).find(z => z.isInside(new Position(x, y))) match {
        case Some(z) => z.name.toInt
        case None => -1
      }
    }

    // finds the zone in which each position of a given pedestrian is
    val pedZonesTimes: Iterable[Vector[(Double, Int)]] = this.ped.values.map(p => {
      p.h_t.zip(p.h_x.zip(p.h_y).map(t => findZone(t._1, t._2))).toVector
    })

    // keeps only the changes in zone and computes the change of zone time as the mean between both time stamps.
    val pedChangeZoneTime: Iterable[Vector[(Double, Int, Int)]] = pedZonesTimes.map(v => v.dropRight(1).zip(v.tail).filter(ids => ids._1._2 != ids._2._2).map(keep => (0.5 * (keep._1._1 + keep._2._1), keep._1._2, keep._2._2)).sortBy(_._1))

    // groups the zone changing by time interval and zone change, then counts the number of people doing the change
    pedChangeZoneTime.flatten.groupBy(t => (findTimesIdx(t._1), t._2, t._3)).map(m => m._1 -> m._2.size)
  }

  type SideOfLine = Int
  final val ONLEFT: SideOfLine = 1
  final val ONRIGHT: SideOfLine = -1
  final val EXACTLYON: SideOfLine = 0

  // INFLOW TO THE ZONE MUST BE DEFINED AS CROSSING FROM LEFT TO RIGHT WHEN STOOD AT ORIGIN POINT
  def computeSideOfLine(point: (Double, Double), start: (Double, Double), end: (Double, Double)): SideOfLine = {
    val value = (end._1 - start._1) * (point._2 - start._2) - (point._1 - start._1) * (end._2 - start._2)
    if (value > 0.0) {
      ONLEFT
    }
    else if (value < 0.0) {
      ONRIGHT
    }
    else {
      ONRIGHT
    } // if on the line, consider it as being inside
  }

  type FlowDirection = Int
  final val INFLOW: FlowDirection = 1
  final val OUTFLOW: FlowDirection = -1


  def computeMvmtsAcrossLine(line: FlowLineDA): Iterable[(Double, FlowDirection)] = {
    this.ped.values.flatMap(pedestrian => {
      val pedPositionExpanded = pedestrian.h_t.zip(pedestrian.h_x.zip(pedestrian.h_y)).dropRight(1).zip(pedestrian.h_t.zip(pedestrian.h_x.zip(pedestrian.h_y)).tail)
      pedPositionExpanded.map(txytxy => (0.5 * (txytxy._1._1 + txytxy._2._1), line.crossesLine(Vector2D(txytxy._1._2._1, txytxy._1._2._2), Vector2D(txytxy._2._2._1, txytxy._2._2._2)))).filter(_._2 != 0)
    })
    /*pedSide.zip(pedSide.drop(1)).filter(ch => ch._1._1 != ch._2._1).map(swap => {
      if (swap._1._1 == ONLEFT && swap._2._1 == ONRIGHT) { (0.5*(swap._1._2+swap._2._2), INFLOW) }
      else if (swap._1._1 == ONRIGHT && swap._2._1 == ONLEFT) { (0.5*(swap._1._2+swap._2._2), OUTFLOW) }
      else {throw new RuntimeException("This case should not happen !")}
    })
  })*/
  }


  def computeFlowsAgain(lines: Vector[FlowLineDA], times: Vector[Double]): (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = {
    val numberTimeIntervals = times.size - 1

    /** Finds the interval corresponding to a specific time in a recursive way.
      * For a specific time t, this will return the index i such that t is in [ times(i), times(i+1) )
      * If t is smaller than times.head, returns -1, if t is larger than times.last, returns -2.
      *
      * @param t time to search location for
      * @return intevral in which the time is located
      *
      */
    //def findTimesIndex(t: Double) = times.indexWhere(_ < t) -1

    def findTimesIdx(t: Double): Int = {

      def helper(currentIdx: Int): Int = {
        if (currentIdx == numberTimeIntervals) -2
        else if (times(currentIdx) <= t && t < times(currentIdx + 1)) currentIdx
        else helper(currentIdx + 1)
      }

      if (t < times.head) -1
      else helper(0)
    }

    val controlledInflow: Map[Int, Int] = lines.filter(_.inflowType == CONTROLLED).flatMap(computeMvmtsAcrossLine).filter(_._2 == INFLOW).groupBy(d => findTimesIdx(d._1)).map(res => res._1 -> res._2.size) - -2 - 1
    val uncontrolledInflow: Map[Int, Int] = lines.filter(_.inflowType == UNCONTROLLED).flatMap(computeMvmtsAcrossLine).filter(_._2 == INFLOW).groupBy(d => findTimesIdx(d._1)).map(res => res._1 -> res._2.size) - -2 - -1
    val outflow: Map[Int, Int] = lines.flatMap(computeMvmtsAcrossLine).filter(_._2 == OUTFLOW).groupBy(d => findTimesIdx(d._1)).map(res => res._1 -> res._2.size) - -2 - -1

    (controlledInflow ++ (for (i <- times.indices.dropRight(1)) yield {
      i -> controlledInflow.getOrElse(i, 0)
    }).toMap,
      uncontrolledInflow ++ (for (i <- times.indices.dropRight(1)) yield {
        i -> uncontrolledInflow.getOrElse(i, 0)
      }).toMap,
      outflow ++ (for (i <- times.indices.dropRight(1)) yield {
        i -> outflow.getOrElse(i, 0)
      }).toMap)
  }


  /** Computes the density, speed and flow based on generalized edie definitions. The idea is to consider a spatial
    * discretization, here one single zone, and count for each time interval the presence of pedestrians inside.
    * This approach also provides a "generalized flow", being the norm of the flow in each direction.
    *
    * In order to avoid the cancellation of opposing flows, the sum of the absoltue value is used.
    *
    * The output format is a vector of Tuples where the first value is the time interval, and the second value
    * is the time spent in the zone, the distance travelled in the x and y directions.
    *
    * For further details, see http://www.sciencedirect.com/science/article/pii/S2352146514001033
    *
    * @param z     zone to consider
    * @param times time intervals
    * @return For each time interval the corresponding time fraction, and travelled distance.
    */
  /*def  computeEdieComponents(z: NewZone, times: Seq[Double]): Vector[((Double, Double), (Double, Double, Double))] = {
    val pedSimplifiedNew: Map[Int, Vector[(Double, (Double, Double))]] = ped.map(p => p._2.ID -> p._2.h_t.toVector.zip(p._2.h_x.toVector.zip(p._2.h_y.toVector)))
    val timeMap: Vector[(Double, Iterable[Int])] = collectIDByTime(times, pedSimplifiedNew, 0.005)

    def helper2(timeMap: Vector[(Double, Iterable[Int])], acc: Vector[((Double, Double), (Double, Double, Double))]): Vector[((Double, Double), (Double, Double, Double))] = {
      if (timeMap.size == 1) acc
      else {
        val IDs: Iterable[Int] = (timeMap.head._2 ++ timeMap.tail.head._2).toVector.distinct
        val toSum: Map[Int, Vector[(Double, (Double, Double))]] = IDs.map(i => i -> pedSimplifiedNew(i).filter(t => timeMap.head._1 <= t._1 && t._1 <= timeMap.tail.head._1 && z.isInside(t._2))).filter(_._2.nonEmpty).toMap
        val t: Iterable[(Double, Double, Double)] = toSum.values.map(p => p.dropRight(1).zip(p.tail).map(t => (t._2._1 - t._1._1, t._2._2._1 - t._1._2._1, t._2._2._2 - t._1._2._2)).foldLeft(0.0, 0.0, 0.0)((acc: (Double, Double, Double), p: (Double, Double, Double)) => (acc._1 + (p._1), acc._2 + (p._2), acc._3 + (p._3))))
        helper2(timeMap.tail, acc :+ ((timeMap.head._1, timeMap.tail.head._1),t.foldLeft(0.0, 0.0, 0.0)((acc: (Double, Double, Double), p: (Double, Double, Double)) => (acc._1 + p._1, acc._2 + abs(p._2), acc._3 + abs(p._3)))))
      }
    }
    helper2(timeMap, Vector())
  }*/

  def computeEdieComponents(z: Rectangle, times: Seq[Double]): Vector[((Double, Double), (Double, Double, Double))] = {
    val pedSimplifiedNew: Map[Int, Vector[(Double, (Double, Double))]] = ped.map(p => p._2.ID -> p._2.h_t.toVector.zip(p._2.h_x.toVector.zip(p._2.h_y.toVector))).toMap
    val timeMap: Vector[(Double, Iterable[Int])] =
      ped.values.flatMap(p => {
        times.collect { case t if {
          val idx: Int = p.h_t.indexWhere(_ > t);
          p.h_t.head <= t && p.h_t.last <= t && idx >= 0 && idx < p.h_t.size - 1
        } => {
          val idx: Int = p.h_t.indexWhere(_ > t)
          (t, p.ID, linearInterpolationPosition((p.h_x(idx), p.h_y(idx)), (p.h_x(idx + 1), p.h_y(idx + 1)), p.h_t(idx), p.h_t(idx + 1), t))
        }
        }.filter(tPos => z.isInside(new Position(tPos._3._1, tPos._3._2)))
      }).groupBy(tup => tup._1).map(tup => (tup._1, tup._2.map(_._2))).toVector.sortBy(_._1)
    //val timeMap: Vector[(Double, Iterable[Int])] = collectIDByTime(times, pedSimplifiedNew, 0.05)
    //println(timeMap)

    times.dropRight(1).zip(times.tail).zip(
      for (i <- times.tail.indices) yield {
        val t1: Double = times(i)
        val t2: Double = times(i + 1)
        ped.values.flatMap(p => {
          val txyData = p.getTXYZipped.filter(txy => t1 <= txy._1 && txy._1 < t2 & z.isInside(new Position(txy._2._1, txy._2._2)))
          if (txyData.nonEmpty) {
            txyData.dropRight(1).zip(txyData.tail).map(tup => (tup._2._1 - tup._1._1, (tup._2._2._1 - tup._1._2._1, tup._2._2._2 - tup._1._2._2))).toVector
          } else {
            Vector()
          }
        }).foldLeft(0.0, 0.0, 0.0)((acc: (Double, Double, Double), p: (Double, (Double, Double))) => (acc._1 + p._1, acc._2 + scala.math.abs(p._2._1), acc._3 + scala.math.abs(p._2._2)))

      }
    ).toVector

    /*def helper2(timeMap: Vector[(Double, Iterable[Int])], acc: Vector[((Double, Double), (Double, Double, Double))]): Vector[((Double, Double), (Double, Double, Double))] = {
      if (timeMap.size == 1) acc
      else {
        val IDs: Iterable[Int] = (timeMap.head._2 ++ timeMap.tail.head._2).toVector.distinct
        val toSum: Map[Int, Vector[(Double, (Double, Double))]] = IDs.map(i => i -> pedSimplifiedNew(i).filter(t => timeMap.head._1 <= t._1 && t._1 < timeMap.tail.head._1 && z.isInside(t._2))).filter(_._2.nonEmpty).toMap
        val t: Iterable[(Double, Double, Double)] = toSum.values.map(pData => {println(pData.last._1 - pData.head._1);(pData.last._1 - pData.head._1, pData.last._2._1 - pData.head._2._1, pData.last._2._2 - pData.head._2._2)})
        helper2(timeMap.tail, acc :+ ((timeMap.head._1, timeMap.tail.head._1), t.foldLeft(0.0, 0.0, 0.0)((acc: (Double, Double, Double), p: (Double, Double, Double)) => (acc._1 + p._1, acc._2 + abs(p._2), acc._3 + abs(p._3)))))
      }
    }
    helper2(timeMap, Vector())*/
  }

  /*def linearRegressionOutflowFraction(outflow: DenseVector[Double], totalFlow: DenseMatrix[Double]): Double = {
    val result: LeastSquaresRegressionResult = leastSquares(totalFlow, outflow)
    println(totalFlow, outflow)
    println("Computed linear regression of outflow VS generalized flow:")
    println(" * slope = " + result.coefficients(0))
    println(" * intercept is fixed to 0")
    println(" * r^2 = " + Double.NaN)
    assert(result.coefficients.activeSize == 1, "active size of coefficients is not 1 !")
    result.coefficients(0)
  }

  def linearRegressionMFD(totalFlow: DenseVector[Double], density: DenseMatrix[Double], xmin: Double = Double.NegativeInfinity, xmax: Double = Double.PositiveInfinity): Double = {
    val indices2Drop = density.toArray.zipWithIndex.filterNot(d => xmin <= d._1 && d._1 <= xmax).map(_._2)
    val x: DenseMatrix[Double] = density.delete(indices2Drop, axis = Axis._0)
    val y: DenseVector[Double] = totalFlow.asDenseMatrix.delete(indices2Drop, axis = Axis._1).toDenseVector
    println(x.rows, x.cols, y.length)
    val result: LeastSquaresRegressionResult = leastSquares(x, y)
    println("Computed linear regression of MFD: density vs total flow:")
    println(" * lower bound = " + xmin + ", upper bound = " + xmax)
    println(" * slope = " + result.coefficients(0))
    println(" * intercept is fixed to 0")
    println(" * r^2 = " + Double.NaN)
    assert(result.coefficients.activeSize == 1, "active size of coefficients is not 1 !")
    result.coefficients(0)
  }

  def computeControllerParams(inflowControlled: DenseVector[Double],
                              inflowUncontrolled: DenseVector[Double],
                              outflow: DenseVector[Double],
                              density: DenseVector[Double],
                              totalFlow: DenseVector[Double],
                              targetDensity: Double,
                              targetInflow: Double): (Double, Double, Double) = {

    // computes ratio between outflow and generalized flow
    val outflowFraction: Double = linearRegressionOutflowFraction(outflow, totalFlow.asDenseMatrix.t)

    // computes linear regression of p-MFD within specified region
    val MFDLinearizedSlope: Double = linearRegressionMFD(totalFlow, density.asDenseMatrix.t, xmin = 0.0, xmax = 1.5)

    // collects data for time-series least-squares fit
    val density_kp1 = density(1 until density.size) - targetDensity
    val density_k = density(0 until density.size - 1) - targetDensity
    val inflow_k = inflowControlled(0 until inflowControlled.size - 1) - targetInflow + inflowUncontrolled(0 until inflowUncontrolled.size - 1)

    Vector(density_kp1.toScalaVector(), density_k.toScalaVector(), inflowControlled(0 until inflowControlled.size - 1).toScalaVector()).writeToCSV("density_inflow.csv")

    // estimates mu and ksi parameters
    val result: LeastSquaresRegressionResult = leastSquares(DenseMatrix(density_k, inflow_k).t, density_kp1)
    val mu: Double = result.coefficients(0)
    val ksi: Double = result.coefficients(1)
    println("Computed PI controller parameters:")
    println(" * mu=" + mu + ", ksi=" + ksi)
    println(" * KP=" + mu / ksi + ", KI=" + (1.0 - mu) / ksi)
    new ScatterPlot3D("densityk-vs-inflowk-vs-densitykp1.png", density_k.toScalaVector(), inflow_k.toScalaVector(), density_kp1.toScalaVector(), "delta density_k", "delta inflow_k", opts = PlotOptions(xTickInterval = 0.1, yTickInterval = 0.05))
    (outflowFraction, mu / ksi, (1.0 - mu) / ksi)
  }*/

  def writePedestriansToJSON(fileName: String): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("[")

    def helper(data: Iterable[Pedestrian]): Unit = {
      if (data.size == 1) {
        bw.write(data.head.toJSON4HubModel)
      } else {
        bw.write(data.head.toJSON4HubModel + ",\n")
        helper(data.tail)
      }
    }

    helper(this.ped.values.toVector.sortBy(_.ID))
    bw.write("]")
    bw.close()
  }

  def countEntriesExitsPerZone(ped: Seq[Pedestrian])(zones: Seq[(Int, Int)]): Map[(Int, Int), Int] = {

    val oCount: collection.mutable.Map[(Int, Int), Int] = collection.mutable.Map().withDefaultValue(0)
    val dCount: collection.mutable.Map[(Int, Int), Int] = collection.mutable.Map().withDefaultValue(0)
    ped.foreach(p => {
      oCount((p.oZone, p.dZone)) += 1
    })
    oCount.toMap.filter(kv => zones.contains(kv._1))
  }

  def countEntriesExitsPerZone(zones: Seq[(Int, Int)]): Map[(Int, Int), Int] = countEntriesExitsPerZone(this.ped.values.toSeq)(zones)

  /** Based on the functions passed as argument, computes the metrics per time window of all the pedestrians satisfying
    * the predicate "filter".
    *
    * @param filter     predicate used to filter the population
    * @param pedFunc    extracts the metric from an individual
    * @param windowFunc computes the index of the window in which the pedestrian belongs
    * @param metricFunc computes the statistics of the metric
    * @return map where the keys are the time intervals and the values the statstics of the metric
    */
  def aggregateMetricByTimeWindow(filter: Pedestrian => Boolean, pedFunc: Pedestrian => Double, windowFunc: Pedestrian => Int, metricFunc: Iterable[Double] => (Int, Double, Double, Double, Double, Double)): Map[Int, (Int, Double, Double, Double, Double, Double)] = {
    this.ped.values.filter(filter).groupBy(windowFunc).map(grouped => grouped._1 -> grouped._2.map(pedFunc).stats)
  }


}
