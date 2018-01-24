package hubmodel.input.demand

/**
  * Created by nicholas on 5/12/17.
  */

import java.time.LocalDateTime
import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.pow
import hubmodel.input.infrastructure.{NodeID, TrackID}
import hubmodel.{Action, NewPosition2D, NewTime, PedestrianDES, PedestrianSim, Position, SFGraphSimulator, Time}

/** Extension of [[Action]] which will insert a [[CreatePedestrian]] actions based on a Poisson distribution for
  * the creation times.
  *
  * @param start        time when pedestrians should start arriving
  * @param end          end time of the pedestrian creation
  * @param numberPeople number of people to create
  */
class PedestrianGeneration(o: NodeID, d: NodeID, start: Double, end: Double, numberPeople: Double, sim: SFGraphSimulator) extends Action {

  /** Poisson distribution
    *
    * @param duration     endTime - startTime
    * @param numberPeople number of start times to generate
    * @param u            random number between 0 and 1 from a uniform distribution
    * @param acc          accumulator to store the results
    * @return Vector of [[Time]] corresponding to the arrival times inside the zone
    */
  @deprecated
  def poissonProcess(duration: Long, numberPeople: Double, u: () => Double, acc: Vector[Time]): Vector[Time] = {
    val rate: Double = numberPeople / duration
    if (acc.isEmpty) poissonProcess(duration, numberPeople, u, Vector(0 - (math.log(u()) / rate)))
    else if (acc.length >= numberPeople) acc
    else poissonProcess(duration, numberPeople, u, acc :+ (acc.last - (math.log(u()) / rate)))
  }

  def poissonProcessIterator(duration: Double, numberPeople: Double): Iterator[NewTime] = {
    val rate: Double = numberPeople / duration
    var t: Double = -math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0)/rate)
    Iterator.continually{ t = t - math.log(ThreadLocalRandom.current.nextDouble(0.0,1.0))/rate; t}.takeWhile(v => v < duration).map(new NewTime(_))
  }


  /** Computes all the arrival times using the [[poissonProcessIterator]] function,
    * then adds new pedestrians at those times.
    *
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": generating " + numberPeople + " pedestrians in interval " + start + ":" + end)
    //println((end-start).toLong, numberPeople, sim.randU, Vector())
    poissonProcessIterator(end - start, numberPeople).foreach(t => sim.insertEventWithDelayNew(t)(new CreatePedestrian(o, d, sim)))
    //arrivalTimes.
  }
}

/** Creates a pedestrian. A new pedestrian will be added when this event is executed.
  *
  */
class CreatePedestrian(o: NodeID, d: NodeID, sim: SFGraphSimulator) extends Action {

  /** Inserts a new pedestrian. The characteristics of this pedestrian are sampled on creation.
    * TODO: improve the generation protection. The code can get stuck here !
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": pedestrian created")
    val route = sim.graph.getShortestPath(sim.graph.vertexMap(o.toString), sim.graph.vertexMap(d.toString)).tail
    val generationPoint: NewPosition2D = {
      var pos: NewPosition2D = sim.graph.vertexMap(o.toString).uniformSamplePointInside
      var it: Int = 0
      while (it < 100 && sim.population.exists(ped => pow((ped.currentPosition(0)-pos._1)*(ped.currentPosition(0)-pos._1) + (ped.currentPosition(1)-pos._2)*(ped.currentPosition(1)-pos._2), 0.5) < 0.5)) {
        pos = sim.graph.vertexMap(o.toString).uniformSamplePointInside
        it = it +1
      }
      pos
    }
    val ped = new PedestrianSim(o, d, sim.currentTime, generationPoint, sim.graph.vertexMap(route.head.name).uniformSamplePointInside, route)
    //println(sim.infraGraph.graph.getShortestPathFunction(sim.infraGraph.vertexMap(o.toString), sim.infraGraph.vertexMap(d.toString)).map(v => v.name))
    sim.append2Population(ped)
  }
}


