package hubmodel.mgmt

import hubmodel.DES.{Action, NOMADGraphSimulator}
import hubmodel.P
import hubmodel.mgmt.flowgate.{FlowGate, FlowGateFunctional}
import hubmodel.ped.PedestrianNOMAD
import tools.Time
import tools.TimeNumeric.mkOrderingOps

import scala.annotation.tailrec

class UpdateGates[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Action {

  def allocateSupply(totalInflow: Double, gates: Vector[String]): Map[String, Double] = {
    gates.map(g => g -> totalInflow / gates.size).toMap
  }

  private def computeReleaseTimes(rate: Double, maxTime: Time): List[Time] = {

    @tailrec
    def inner(accum: List[Time]): List[Time] = {
      if (accum.head >= maxTime) accum
      else {
        inner(accum.head.addDouble(1.0 / rate) :: accum)
      }
    }

    inner(List(Time(1.0 / rate)))
  }

  override def execute(): Unit = {
    sim.eventLogger.trace("time: " + sim.currentTime + ": updating flow gates using DLQR")

    sim.controlDevices.flowGates.foreach(fgGen => {
      fgGen match {
        case fg: FlowGateFunctional[_, _] => {

          fg.functionalForm match {
            case density: FunctionalFormGating => {

              //density.f(Density(1.0))

              // pax above target density
              fg.setFlowRate(
                density.f(Density(sim.criticalAreas(fg.monitoredArea).paxIndividualDensityHistory.last._2.count(_ > sim.criticalAreas(fg.monitoredArea).targetDensity))).f,
                sim.currentTime
              )

              // mean voronoi density
              /*fg.flowRate = math.min(fg.functionalForm(sim.criticalAreas(fg.monitoredArea).targetDensity - sim.criticalAreas(fg.monitoredArea).densityHistory.last._2), 10.0)*/
            }
          }
        }
        case fg: FlowGate => {
          val totalInflow: Double = math.max(0.1, math.min(5.0, 0.65 * (sim.criticalAreas(fg.monitoredArea).targetDensity - sim.criticalAreas(fg.monitoredArea).densityHistory.last._2)))
          fg.setFlowRate(
            math.min(fg.width * 1.5, fg.width * (totalInflow / sim.controlDevices.flowGates.count(_.monitoredArea == fg.monitoredArea))),
            sim.currentTime
          )
        }
      }

      // when execution of release pedestrian takes place, if flow rate is 0 then the event will never happen. Hence manually insert one to restart flow gates.
      if (fgGen.flowRate > 0.0) {
        //sim.errorLogger.error("release times at: " + sim.currentTime + ", rate=" + fgGen.flowRate + ", times=" + computeReleaseTimes(fgGen.flowRate, sim.evaluate_dt))
        computeReleaseTimes(fgGen.flowRate, sim.evaluate_dt).foreach(t => sim.insertEventWithDelay(t)(new fgGen.ReleasePedestrian(sim)))
      }
    })
  }

  type A = UpdateGates[P]

  override def deepCopy(simulator: NOMADGraphSimulator[P]): Option[A] = {
    Some(new UpdateGates(simulator))
  }
}

