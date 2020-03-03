package hubmodel.demand.flows

/**
  * Created by nicholas on 5/12/17.
  */

import java.util.concurrent.ThreadLocalRandom

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.demand.{CreatePedestrian, CreatePedestrianWithInsertion}
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.TimeNumeric.mkOrderingOps
import tools.cells.{Rectangle, Vertex}

import scala.reflect.ClassTag

/** Extension of [[Action]] which will insert a [[CreatePedestrian]] actions based on the non homogeneous arrival rate
  * of pedestrians. This action inserts only the next [[CreatePedestrianWithInsertion]] event. The insertion time
  * generator is passed to the avent so it inserts the next one.
  *
  * @param o            origin node
  * @param d            destination node
  * @param start        beginning of interval
  * @param end          end of interval
  * @param rateFunction non homogenous function
  * @param sim          simulator
  */
class PedestrianGenerationNonHomogeneousRate(o: Vertex, d: Vertex, start: Time, end: Time, rateFunction: Time => Double, sim: NOMADGraphSimulator)(implicit tag: ClassTag[PedestrianNOMAD]) extends Action {

  /** Maximum rate of the pedestrian generation rate, computed by sampling */
  private val rateMax: Double = start.value.to(end.value).by(0.01).map(v => rateFunction(Time(v.toDouble))).max

  //private var previousGenerationTime: Time = Time(0)

  def next(currentSimulationTime: Time): Option[Time] = {
    var interval: Time = Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0)) / rateMax)

    if (currentSimulationTime < this.end) {
      var s: Double = ThreadLocalRandom.current.nextDouble(0.0, 1.0)
      while (s > rateFunction(currentSimulationTime) / rateMax) {
        interval = interval + Time(-math.log(ThreadLocalRandom.current.nextDouble(0.0, 1.0)) / rateMax)
        s = ThreadLocalRandom.current.nextDouble(0.0, 1.0)
      }
      Some(interval)
    } else {
      None
    }
  }

  /**
    *
    */
  override def execute(): Unit = {

    // add entry in log file
    sim.eventLogger.trace("time=" + sim.currentTime + ": processing pedestrian flow and inserting first pedestrian" + start + ":" + end)

    // Samples all the times and then inserts [[CreatePedestrian]] events at those times.
    this.next(sim.currentTime) collect {
      case t: Time => sim.insertEventAtAbsolute(start + t)(new CreatePedestrianWithInsertion(o, d, sim, this.next))
    }
  }

  type A = PedestrianGenerationNonHomogeneousRate

  override def deepCopy(simulator: NOMADGraphSimulator): Option[A] = Some(new PedestrianGenerationNonHomogeneousRate(this.o, this.d, this.start, this.end, this.rateFunction, simulator))
}





