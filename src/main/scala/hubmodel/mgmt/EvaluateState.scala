package hubmodel.mgmt

import hubmodel.{Action, SFGraphSimulator, Vector2D, isInVertex}
import kn.uni.voronoitreemap.datastructure.OpenList
import kn.uni.voronoitreemap.diagram.PowerDiagram
import kn.uni.voronoitreemap.j2d.{PolygonSimple, Site}

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
    val paxInZone: Int = sim.population.count(ped => isInVertex(sim.criticalArea.head)(ped.currentPositionNew))
    //sim.densityHistory.append((sim.currentTime, paxInZone/sim.criticalArea.head.area))
    // voronoin density
    if (paxInZone > 0) {
      try {
        val voronoi: PowerDiagram = new PowerDiagram()
        val stupidList: OpenList = new OpenList()
        sim.population.map(p => new Site(p.currentPositionNew.X, p.currentPositionNew.Y)).foreach(stupidList.add)
        voronoi.setSites(stupidList)
        val box: PolygonSimple = new PolygonSimple
        box.add(sim.criticalArea.head.A.X, sim.criticalArea.head.A.Y) //21.440, 10.720)
        box.add(sim.criticalArea.head.B.X, sim.criticalArea.head.B.Y) //80.400, 10.720)
        box.add(sim.criticalArea.head.C.X, sim.criticalArea.head.C.Y) //80.400, 16.800)
        box.add(sim.criticalArea.head.D.X, sim.criticalArea.head.D.Y) //21.440, 16.800)
        voronoi.setClipPoly(box)
        sim.densityHistory.append(
          (sim.currentTime, voronoi.computeDiagram().filter(s => isInVertex(sim.criticalArea.head)(Vector2D(s.x, s.y))).foldLeft(0.0)((acc: Double, n: Site) => acc + 1.0 / (paxInZone * n.getPolygon.getArea)))
        )
      } catch {
        case e: Exception => {
          sim.errorLogger.warn("sim-time=" + sim.currentTime + "exception when computing voronoi diagram, using standard density! " + sim.population.size + " people in sim and " + sim.population.count(p => isInVertex(sim.criticalArea.head)(p.currentPositionNew)) + " in box")
          sim.densityHistory.append((sim.currentTime, sim.population.count(p => isInVertex(sim.criticalArea.head)(p.currentPositionNew)).toDouble / sim.criticalArea.head.area))
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
    if (sim.useFlowGates) sim.insertEventWithZeroDelay(new PIGateController(sim))
    sim.insertEventWithDelayNew(sim.evaluate_dt)(new EvaluateState(sim))
  }
}
