package hubmodel.demand

import breeze.numerics.pow
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel._
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.Rectangle

/**
  * Creates a pedestrian. A new pedestrian will be added when this event is executed.
  */
class CreatePedestrianWithInsertion(o: Rectangle, d: Rectangle, sim: SFGraphSimulator, timeGenerator: Time => Option[Time]) extends Action {

  /**
    * Inserts a new pedestrian. The characteristics of this pedestrian are sampled on creation.
    * The insertion point of the pedestrian is sampled inside the zone until it is not too close to any other
    * pedestrian. Maximum one hundred iterations are performed, hence if the zone is too crowded, pedestrians
    * will start being pushed away from each other very violently.
    *
    * If problems inside zone on creation this is a good place to start.
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": pedestrian created")

    // the shortest path method returns the origin node as the first element of the route.
    val route = sim.graph.getShortestPath(o, d).tail

    // sample until the generation point is far enough away from other pedestrians
    val generationPoint: Position = {
      var pos: Position = o.uniformSamplePointInside
      var it: Int = 0
      while (it < 100 && sim.population.exists(ped => pow((ped.currentPosition.X - pos.X) * (ped.currentPosition.X - pos.X) + (ped.currentPosition.Y - pos.Y) * (ped.currentPosition.Y - pos.Y), 0.5) < 0.5)) {
        pos = o.uniformSamplePointInside
        it = it + 1
      }
      pos
    }

    // inserts new pedestrian into population
    sim.insertInPopulation(new PedestrianNOMAD(o, d, sim.currentTime, generationPoint, route))

    timeGenerator(sim.currentTime) collect {
      case t: Time => {
        sim.insertEventWithDelayNew(t) (new CreatePedestrianWithInsertion (o, d, sim, timeGenerator) )
      }
    }
  }
}