package hubmodel.mgmt

import breeze.linalg.DenseVector
import breeze.numerics.abs
import hubmodel.{Action, SFGraphSimulator, isInVertex}
import kn.uni.voronoitreemap.datastructure.OpenList
import kn.uni.voronoitreemap.diagram.PowerDiagram
import kn.uni.voronoitreemap.j2d.Site
import kn.uni.voronoitreemap.j2d.PolygonSimple

import scala.collection.JavaConversions._

/**
  * Created by nicholas on 5/13/17.
  */

/** Class which evaluates the various indicators required by the management strategies. The indicators are stored
  * in the simulator itslef. This way, all strategies can easily acccess the indictros needed.
  *
  * @param sim simulation containing the data
  */
class EvaluateState(sim: SFGraphSimulator) extends Action with Controller {

  /** Method called by the simulation to perform the action. Here, the KPIs are evaluted.
    *
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": state evaluation")

    // computes the number of people inside the zone(s) to control.
    val paxInZone: Int = sim.population.count(ped => isInVertex(sim.criticalArea.head)(ped.currentPosition))
    //sim.densityHistory.append((sim.currentTime, paxInZone/sim.criticalArea.head.area))
    // voronoin density
    if (paxInZone > 0) {
      try {
        val voronoi: PowerDiagram = new PowerDiagram()
        val stupidList: OpenList = new OpenList()
        sim.population.map(p => new Site(p.currentPosition(0), p.currentPosition(1)) ).foreach(stupidList.add)
        voronoi.setSites(stupidList)
        val box: PolygonSimple = new PolygonSimple
        box.add(sim.criticalArea.head.A(0), sim.criticalArea.head.A(1)) //21.440, 10.720)
        box.add(sim.criticalArea.head.B(0), sim.criticalArea.head.B(1)) //80.400, 10.720)
        box.add(sim.criticalArea.head.C(0), sim.criticalArea.head.C(1)) //80.400, 16.800)
        box.add(sim.criticalArea.head.D(0), sim.criticalArea.head.D(1)) //21.440, 16.800)
        voronoi.setClipPoly(box)
        sim.densityHistory.append(
          (sim.currentTime, voronoi.computeDiagram().filter(s => isInVertex(sim.criticalArea.head)(DenseVector(s.x, s.y))).foldLeft(0.0)((acc: Double, n: Site) => acc + 1.0 / (paxInZone * n.getPolygon.getArea)))
        )
      } catch {
        case e : Exception => {
          sim.errorLogger.warn("sim-time=" + sim.currentTime + "exception when computing voronoi diagram, using standard density! " + sim.population.size + " people in sim and " + sim.population.count(p => isInVertex(sim.criticalArea.head)(p.currentPosition)) + " in box")
          sim.densityHistory.append( (sim.currentTime, sim.population.count(p => isInVertex(sim.criticalArea.head)(p.currentPosition)).toDouble/sim.criticalArea.head.area) )
        }
      }
    }
    else {
      sim.densityHistory.append(
        (sim.currentTime, 0.0)
      )
    }
    sim.eventLogger.trace("sim-time=" + sim.currentTime + ": number people inside critical area: " + sim.densityHistory.last._2)
    //sim.insertEventWithDelay(0) (new MoveGates)
    if (sim.useFlowGates) sim.insertEventWithDelay(0) ( new PIGateController(sim) )
    sim.insertEventWithDelay(sim.evaluate_dt) ( new EvaluateState(sim) )
  }
}
