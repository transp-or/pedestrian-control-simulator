package hubmodel.demand

/**
  * Created by nicholas on 5/12/17.
  */

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel._
import hubmodel.ped.PedestrianNOMAD
import hubmodel.tools.cells.Rectangle

import scala.reflect.ClassTag

/** Extension of [[Action]] which will insert a [[CreatePedestrian]] actions based on a Poisson distribution for
  * the creation times.
  *
  * @param start        time when pedestrians should start arriving
  * @param end          end time of the pedestrian creation
  * @param numberPeople number of people to create
  */
class PedestrianGeneration[T <: PedestrianNOMAD](o: Rectangle, d: Rectangle, start: Time, numberPeople: Int, sim: NOMADGraphSimulator[T])(implicit tag: ClassTag[T]) extends Action[T] {

  /** Poisson distribution
    *
    * @param duration     endTime - startTime
    * @param numberPeople number of start times to generate
    * @return Vector of [[Time]] corresponding to the arrival times inside the zone
    */
  def poissonProcessIterator(duration: Double, numberPeople: Double): Iterator[Time] = {
    val rate: Double = numberPeople / duration
    var t: Double = -math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / rate)
    Iterator.continually {
      t = t - math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0)) / rate; t
    }.takeWhile(v => v < duration).map(new Time(_))
  }


  /** Computes all the arrival times using the [[poissonProcessIterator]] function,
    * then adds new pedestrians at those times.
    *
    */
  override def execute(): Unit = {
    sim.eventLogger.trace("time=" + sim.currentTime + ": generating " + numberPeople + " pedestrians at " + start)
    //println((end-start).toLong, numberPeople, sim.randU, Vector())
    //poissonProcessIterator(end.value-start.value, numberPeople).foreach(t => {sim.insertEventWithDelayNew(add(start,t))(new CreatePedestrian(o, d, sim))})
    val tinfQueue: PTInducedQueue[T] = sim.PTInducedFlows.getOrElseUpdate(o, new PTInducedQueue(o))
    if (tinfQueue.isEmpty) {
      sim.insertEventWithDelay(new Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0) / sim.PTInducedFlows(o).rate))) {
        new ReleasePedPTInducedFlow(o, sim)
      }
    }
    tinfQueue.appendPeds(Vector.fill((1.5*numberPeople).toInt)
    {
      new CreatePedestrian(o, d, sim)


    })
    //arrivalTimes.
  }

  //override def toString: NodeIDOld = this.o + ", " + this.d + ", " + this.start + ", " + this.numberPeople
}




