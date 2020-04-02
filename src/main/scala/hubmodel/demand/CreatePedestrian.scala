package hubmodel.demand

import hubmodel.DES.{Action, NOMADGraphSimulator, PedestrianPrediction}
import hubmodel._
import hubmodel.ped.PedestrianNOMAD
import tools.cells.Vertex

import scala.reflect.ClassTag

/**
  * Creates a pedestrian. A new pedestrian will be added when this event is executed.
  */
class CreatePedestrian(o: Vertex, d: Vertex, val isTransfer: Boolean, sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

  /**
    * Inserts a new pedestrian. The characteristics of this pedestrian are sampled on creation.
    * The insertion point of the pedestrian is sampled inside the zone until it is not too close to any other
    * pedestrian. Maximum one hundred iterations are performed, hence if the zone is too crowded, pedestrians
    * will start being pushed away from each other very violently.
    *
    * If problems inside zone on creation this is a good place to start.
    */
  override def execute(): String = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": pedestrian created")

    // the shortest path method returns the origin node as the first element of the route.

    //val route = sim.graph.getShortestPath(o, d).tail

    // sample until the generation point is far enough away from other pedestrians
    val generationPoint: Position = {
      var pos: Position = o.uniformSamplePointInside
      var it: Int = 0
      while (it < 100 && sim.population.exists(ped => math.pow((ped.currentPosition.X - pos.X) * (ped.currentPosition.X - pos.X) + (ped.currentPosition.Y - pos.Y) * (ped.currentPosition.Y - pos.Y), 0.5) < 0.5)) {
        pos = o.uniformSamplePointInside
        it = it + 1
      }
      pos
    }

    //tag.runtimeClass.getConstructor(classOf[(Rectangle, Rectangle, Time, Position, List[Rectangle], String)]).newInstance(o, d, sim.currentTime, generationPoint, route, "").asInstanceOf[T]
    // inserts new pedestrian into population

    val newPed: PedestrianNOMAD = new PedestrianNOMAD(o, d, sim.currentTime, generationPoint, sim.logFullPedestrianHistory, isTransfer)
    sim.setFirstRoute(this.sim.currentTime, newPed)
    newPed.updateDesiredDirection()
    newPed.currentVelocity = newPed.desiredDirection * newPed.desiredWalkingSpeed
    newPed.updateClosestWalls(sim.walls)

    sim.insertInPopulation(newPed) //new T(o, d, sim.currentTime, generationPoint, route, ""))

    newPed.ID
  }

  type A = CreatePedestrian

  type B = NOMADGraphSimulator

override def deepCopy(simulator: PedestrianPrediction): Option[A] = Some(new CreatePedestrian(this.o, this.d, this.isTransfer, simulator))
}

