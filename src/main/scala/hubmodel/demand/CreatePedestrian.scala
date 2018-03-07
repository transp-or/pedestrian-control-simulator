package hubmodel.demand

import breeze.numerics.pow
import hubmodel._

/** Creates a pedestrian. A new pedestrian will be added when this event is executed.
  *
  */
class CreatePedestrian(o: VertexRectangle, d: VertexRectangle, sim: SFGraphSimulator) extends Action {

  /** Inserts a new pedestrian. The characteristics of this pedestrian are sampled on creation.
    * TODO: improve the generation protection. The code can get stuck here !
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": pedestrian created")
    val route = sim.graph.getShortestPath(o, d).tail
    val generationPoint: NewBetterPosition2D = {
      var pos: NewBetterPosition2D = o.uniformSamplePointInside
      var it: Int = 0
      while (it < 100 && sim.population.exists(ped => pow((ped.currentPositionNew.X-pos.X)*(ped.currentPositionNew.X-pos.X) + (ped.currentPositionNew.Y-pos.Y)*(ped.currentPositionNew.Y-pos.Y), 0.5) < 0.5)) {
        pos = o.uniformSamplePointInside
        it = it +1
      }
      pos
    }
    val ped = new PedestrianSim(o, d, sim.currentTime, generationPoint, sim.graph.vertexMap(route.head.name).uniformSamplePointInside, route)
    //println(sim.infraGraph.graph.getShortestPathFunction(sim.infraGraph.vertexMap(o.toString), sim.infraGraph.vertexMap(d.toString)).map(v => v.name))
    sim.append2Population(ped)
  }
}