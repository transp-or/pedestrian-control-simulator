package hubmodel.control

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import hubmodel.control.amw.{MovingWalkwayWithDensityMeasurement, MovingWalkwayWithFlowMeasurement}
import kn.uni.voronoitreemap.datastructure.OpenList
import kn.uni.voronoitreemap.diagram.PowerDiagram
import kn.uni.voronoitreemap.j2d.{PolygonSimple, Site}
import myscala.math.vector.Vector2D
import tools.cells.isInVertex

import java.util.concurrent.ThreadLocalRandom
import scala.jdk.CollectionConverters._

/**
  * Created by nicholas on 5/13/17.
  */

/** Class which evaluates the various indicators required by the management strategies. The indicators are stored
  * in the simulator itslef. This way, all strategies can easily acccess the indictros needed.
  *
  * @param sim simulation containing the data
  */
abstract class EvaluateState(sim: NOMADGraphSimulator) {

  def computeDensityAtCurrentTime(): Unit = {

    sim.criticalAreas.values.foreach(zone => {

      // computes the number of people inside the zone(s) to control and stores the entrance and exit time
      // of the pedestrian in/out from this zone
      val nbrPaxInZone: Int = sim.population.count(ped => isInVertex(zone)(ped.currentPosition))

      // voronoi density
      if (nbrPaxInZone > 0) {
        try {
          val voronoi: PowerDiagram = new PowerDiagram()
          val stupidList: OpenList = new OpenList()
          sim.population.map(p => new Site(p.currentPosition.X, p.currentPosition.Y)).foreach(stupidList.add)
          voronoi.setSites(stupidList)
          val box: PolygonSimple = new PolygonSimple
          zone.corners.foreach(corner => box.add(corner.X, corner.Y))
          voronoi.setClipPoly(box)
          val voronoiTessellations: Vector[Site] = voronoi.computeDiagram().asScala.toVector

          sim.densityMeasurementError match {
            case Some(e) => { // if a measurement error must be applied, compute the density values with the error
              val densityValues: Vector[(Double, Double)] = voronoiTessellations
                .filter(s => isInVertex(zone)(Vector2D(s.x, s.y)))
                .map(p => 1.0 / p.getPolygon.getArea)
                .map(d => {
                  var erroredDensity: Double = d + (d * e.varianceFraction * ThreadLocalRandom.current().nextGaussian())
                  while (erroredDensity <= 0.0) {
                    erroredDensity = d + (d * e.varianceFraction * ThreadLocalRandom.current().nextGaussian())
                  }
                  (d, erroredDensity)
                })

              zone.densityHistoryCorrect.append(
                (sim.currentTime, densityValues.foldLeft(0.0)((acc: Double, p: (Double, Double)) => acc + (p._1 / nbrPaxInZone)))
              )

              zone.densityHistory.append(
                (sim.currentTime, densityValues.foldLeft(0.0)((acc: Double, p: (Double, Double)) => acc + (p._2 / nbrPaxInZone)))
              )

              zone.paxIndividualDensityHistory.append(
                (sim.currentTime, densityValues.map(_._2))
              )

              zone.paxIndividualDensityHistoryCorrect.append(
                (sim.currentTime, densityValues.map(_._1))
              )
            }
            case None => { // no density measurement errors
              zone.densityHistory.append(
                (sim.currentTime, voronoiTessellations.filter(s => isInVertex(zone)(Vector2D(s.x, s.y))).foldLeft(0.0)((acc: Double, n: Site) => acc + 1.0 / (nbrPaxInZone * n.getPolygon.getArea)))
              )

              zone.densityHistoryCorrect.append(
                (sim.currentTime, voronoiTessellations.filter(s => isInVertex(zone)(Vector2D(s.x, s.y))).foldLeft(0.0)((acc: Double, n: Site) => acc + 1.0 / (nbrPaxInZone * n.getPolygon.getArea)))
              )

              zone.paxIndividualDensityHistory.append(
                (sim.currentTime, voronoiTessellations.filter(s => isInVertex(zone)(Vector2D(s.x, s.y))).map(1.0 / _.getPolygon.getArea))
              )

              zone.paxIndividualDensityHistoryCorrect.append(
                (sim.currentTime, voronoiTessellations.filter(s => isInVertex(zone)(Vector2D(s.x, s.y))).map(1.0 / _.getPolygon.getArea))
              )
            }
          }
        } catch {
          case e: Exception => {
            //sim.errorLogger.warn("sim-time=" + sim.currentTime + "exception when computing voronoi diagram, using standard density! " + sim.population.size + " people in sim and " + sim.population.count(p => isInVertex(zone)(p.currentPosition)) + " in box")
            zone.densityHistory.append((sim.currentTime, sim.population.count(p => isInVertex(zone)(p.currentPosition)).toDouble / zone.area))
          }
        }
      }
      else {
        zone.densityHistory.append(
          (sim.currentTime, 0.0)
        )
      }

    })
  }

  /**
    * Updates the positions of the flow separators based on the incoming flows.
    */
  def processIncomingFlowsForFS(): Unit = {
    sim.controlDevices.flowSeparators.foreach(fs => {
      fs.updateWallTargetPosition(sim.currentTime)
      if (!fs.movingWallEventIsInserted && fs.flowSeparatorNeedsToMove(sim.motionModelUpdateInterval)) {
        sim.insertEventWithZeroDelay(new fs.MoveFlowSeperator(sim))
      }
      //fs.inflowLinesStart.foreach(_.reinitialize())
      //fs.inflowLinesEnd.foreach(_.reinitialize())
    })
  }

  def updateReactiveAMWs(): Unit = {
    // update the speeds
    sim.controlDevices.amws.collect{
      case w: MovingWalkwayWithFlowMeasurement[_,_] => {
        w.updateReactivePolicy(sim.currentTime, sim)
      }
      case wd: MovingWalkwayWithDensityMeasurement[_,_] => {
        wd.updateDirection(sim.currentTime)
        //wd.updateReactivePolicy(sim.currentTime, sim)
      }
    }
  }

}

class UpdateDensityReactiveAMWs(sim: NOMADGraphSimulator) extends Action {

  override val priority: Int = 108

  override def execute(): Any = {

    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": updating density based reactive amws")

    sim.controlDevices.amws.collect{
      case wd: MovingWalkwayWithDensityMeasurement[_,_] => {
        wd.updateReactivePolicy(sim.currentTime, sim)
      }
    }

    sim.trackDensityInterval.foreach(t => sim.insertEventWithDelay(t)(new UpdateDensityReactiveAMWs(this.sim)))

  }

  type A = ComputePedestrianDensity

  override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
}

class ComputePedestrianDensity(sim: NOMADGraphSimulator) extends EvaluateState(sim) with Action {

  override val priority: Int = 109

  override def execute(): Any = {

    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": density computation")

    computeDensityAtCurrentTime()

    sim.trackDensityInterval.foreach(t => sim.insertEventWithDelay(t)(new ComputePedestrianDensity(this.sim)))

  }

  type A = ComputePedestrianDensity

  override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
}

class ReinitializeFlowCounters(sim: NOMADGraphSimulator) extends  Action {

  override val priority: Int = 110

  override def execute(): Any = {
    println(sim.currentTime)
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": reinitialize flow counters")

    // reinitialize the flow counters for the next interval
    sim.controlDevices.amws.collect{
      case w: MovingWalkwayWithFlowMeasurement[_,_] => {

        w.inflowLinesEnd ++ w.inflowLinesStart
      }
      case w: MovingWalkwayWithDensityMeasurement[_,_] => {
        w.updateFlowHistory(sim.currentTime)
        w.inflowLinesEnd ++ w.inflowLinesStart
      }
    }.flatten.foreach(_.fl.reinitialize())


    sim.resetFlowCountersInterval.foreach(t => sim.insertEventWithDelay(t)(new ReinitializeFlowCounters(this.sim)))

  }

  type A = ReinitializeFlowCounters

  override def deepCopy(simulator: PedestrianPrediction): Option[A] = None
}
