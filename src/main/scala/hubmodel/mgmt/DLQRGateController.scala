package hubmodel.mgmt

import breeze.linalg.{max, min}
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.Time
import hubmodel.supply.graph.{FlowGate, FlowGateFunctional}

import scala.annotation.tailrec

class DLQRGateController(sim: SFGraphSimulator) extends Action {

  def allocateSupply(totalInflow: Double, gates: Vector[String]): Map[String, Double] = {
    gates.map(g => g -> totalInflow / gates.size).toMap
  }

  private def computeReleaseTimes(rate: Double, maxTime: Time): List[Time] = {

    @tailrec
    def inner(accum: List[Time]): List[Time] = {
      if (accum.head >= maxTime) accum
      else {inner(accum.head.addDouble(1.0/rate) :: accum)}
    }
    inner(List(Time(1.0/rate)))
  }

  override def execute(): Unit = {
    sim.eventLogger.trace("time: " + sim.currentTime + ": updating flow gates using DLQR")
    //val totalInflow: Double = max(0.1, min(3.0, 0.54*(1.0 - sim.densityHistory.last._2) + 0.0897 * (sim.regulatorIntegralAction + (1.0 - sim.densityHistory.last._2) )))
    //sim.regulatorIntegralAction = sim.regulatorIntegralAction + (1.0 - sim.densityHistory.last._2)

    //sim.inflowHistory.append((sim.currentTime, totalInflow))
    //val flowGatesTotalWidth: Double = sim.controlDevices.flowGates.foldLeft(0.0) { (o: Double, n: FlowGate) => o + n.width }

    sim.controlDevices.flowGates.foreach(fgGen => {
      fgGen match {
        case fg: FlowGateFunctional => {
          println(fg.functionalForm(sim.criticalAreas(fg.monitoredArea).targetDensity - sim.criticalAreas(fg.monitoredArea).densityHistory.last._2))
          fg.flowRate = math.min(fg.functionalForm(sim.criticalAreas(fg.monitoredArea).targetDensity - sim.criticalAreas(fg.monitoredArea).densityHistory.last._2), 10.0)
        }
        case fg: FlowGate => {
          val totalInflow: Double = max(0.1, min(5.0, 0.65 * (sim.criticalAreas(fg.monitoredArea).targetDensity - sim.criticalAreas(fg.monitoredArea).densityHistory.last._2)))
          //println("PI data @ " + sim.currentTime + ", " + sim.criticalAreas(fg.monitoredArea).densityHistory.last._2 + ", " + totalInflow)
          //sim.criticalAreas(fg.monitoredArea).regulatorIntegralAction = sim.criticalAreas(fg.monitoredArea).regulatorIntegralAction + (sim.criticalAreas(fg.monitoredArea).targetDensity - sim.densityHistory.last._2)

          fg.flowRate = min(fg.width * 1.5, fg.width * (totalInflow / sim.controlDevices.flowGates.count(_.monitoredArea == fg.monitoredArea)))
        }
      }

      // when execution of release pedestrian takes place, if flow rate is 0 then the event will never happen. Hence manually insert one to restart flow gates.
      if (fgGen.flowRate > 0.0) {
        try {
          sim.errorLogger.error("release times at: " + sim.currentTime + ", rate=" + fgGen.flowRate + ", times=" + computeReleaseTimes(fgGen.flowRate, sim.evaluate_dt))
          computeReleaseTimes(fgGen.flowRate, sim.evaluate_dt).foreach(t => sim.insertEventWithDelayNew(t)(new fgGen.ReleasePedestrian(sim)))
        } catch {
          case f: Exception => {
            println("error when computing insertion times: " + fgGen.flowRate)
            throw f
          }
        }
      }

    })
  }
}

