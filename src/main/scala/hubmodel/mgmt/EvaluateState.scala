package hubmodel.mgmt

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.isInVertex
import kn.uni.voronoitreemap.datastructure.OpenList
import kn.uni.voronoitreemap.diagram.PowerDiagram
import kn.uni.voronoitreemap.j2d.{PolygonSimple, Site}
import myscala.math.vector.Vector2D

import scala.collection.JavaConversions._

/**
  * Created by nicholas on 5/13/17.
  */

/** Class which evaluates the various indicators required by the management strategies. The indicators are stored
  * in the simulator itslef. This way, all strategies can easily acccess the indictros needed.
  *
  * @param sim simulation containing the data
  */
class EvaluateState[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action[T] with Controller {

  def computeDensity(): Unit = {

    sim.criticalAreas.values.foreach(zone => {

      // computes the number of people inside the zone(s) to control.
      val paxInZone: Int = sim.population.count(ped => isInVertex(zone)(ped.currentPosition))

      // voronoi density
      if (paxInZone > 0) {
        try {
          val voronoi: PowerDiagram = new PowerDiagram()
          val stupidList: OpenList = new OpenList()
          sim.population.map(p => new Site(p.currentPosition.X, p.currentPosition.Y)).foreach(stupidList.add)
          voronoi.setSites(stupidList)
          val box: PolygonSimple = new PolygonSimple
          zone.corners.foreach(corner => box.add(corner.X, corner.Y))
          voronoi.setClipPoly(box)
          val voronoiTessellations: Vector[Site] = voronoi.computeDiagram().toVector
          zone.densityHistory.append(
            (sim.currentTime, voronoiTessellations.filter(s => isInVertex(zone)(Vector2D(s.x, s.y))).foldLeft(0.0)((acc: Double, n: Site) => acc + 1.0 / (paxInZone * n.getPolygon.getArea)))
          )
          zone.paxIndividualDensityHistory.append(
            (sim.currentTime, voronoiTessellations.filter(s => isInVertex(zone)(Vector2D(s.x, s.y))).map(1.0/_.getPolygon.getArea))
          )
        } catch {
          case e: Exception => {
            sim.errorLogger.warn("sim-time=" + sim.currentTime + "exception when computing voronoi diagram, using standard density! " + sim.population.size + " people in sim and " + sim.population.count(p => isInVertex(zone)(p.currentPosition)) + " in box")
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
  def processIncomingFlows(): Unit = {
    // TODO: Take into account limits and hysteresis loop
    sim.controlDevices.flowSeparators.foreach(fs => {
      //println(fs.inflowLinesStart.map(_.getPedestrianFlow).sum.toDouble, fs.inflowLinesEnd.map(_.getPedestrianFlow).sum.toDouble, fs.inflowLinesStart.map(_.getPedestrianFlow).sum.toDouble / fs.inflowLinesEnd.map(_.getPedestrianFlow).sum.toDouble)

    })
  }

  /** Method called by the simulation to perform the action. Here, the KPIs are evaluated.
    *
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": state evaluation")
    this.computeDensity()
    if (sim.useFlowGates || sim.useBinaryGates) { sim.insertEventWithZeroDelay(new DLQRGateController(sim)) }
    if (sim.useFlowSep) {
      //processIncomingFlows()
      sim.controlDevices.flowSeparators.foreach(fs => {
        fs.updateWallTargetPosition(sim.currentTime)
        if ( !fs.movingWallEventIsInserted && fs.flowSeparatorNeedsToMove(sim.sf_dt) ) { sim.insertEventWithZeroDelay(new fs.MoveFlowSeperator(sim)) }
        fs.inflowLinesStart.foreach(_.reinitialize())
        fs.inflowLinesEnd.foreach(_.reinitialize())
      })
    }
    sim.insertEventWithDelay(sim.evaluate_dt)(new EvaluateState(sim))
  }

}
