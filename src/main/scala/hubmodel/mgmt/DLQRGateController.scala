package hubmodel.mgmt

import breeze.linalg.{max, min}
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.Time
import hubmodel.supply.graph.FlowGate

class DLQRGateController(sim: SFGraphSimulator) extends Action {

  def allocateSupply(totalInflow: Double, gates: Vector[String]): Map[String, Double] = {
    gates.map(g => g -> totalInflow / gates.size).toMap
  }

  def computeReleaseTimes(rate: Double, maxTime: Time, currentTime: Time, acc: List[Time]): List[Time] = {
    if (currentTime >= maxTime) acc
    else computeReleaseTimes(rate, maxTime, currentTime.addDouble(1.0 / rate), currentTime.addDouble(1.0 / rate) :: acc)
  }

  override def execute(): Unit = {
    sim.eventLogger.trace("time: " + sim.currentTime + ": updating flow gates using DLQR")
    //val totalInflow: Double = max(0.1, min(3.0, 0.54*(1.0 - sim.densityHistory.last._2) + 0.0897 * (sim.regulatorIntegralAction + (1.0 - sim.densityHistory.last._2) )))
    //sim.regulatorIntegralAction = sim.regulatorIntegralAction + (1.0 - sim.densityHistory.last._2)

    //sim.inflowHistory.append((sim.currentTime, totalInflow))
    //val flowGatesTotalWidth: Double = sim.controlDevices.flowGates.foldLeft(0.0) { (o: Double, n: FlowGate) => o + n.width }

    sim.controlDevices.flowGates.foreach(fg => {
      val totalInflow: Double = max(0.1, min(5.0, 0.65*(1.4 - sim.criticalAreas(fg.monitoredArea).densityHistory.last._2)))
      println("PI data @ "+ sim.currentTime + ", " + sim.criticalAreas(fg.monitoredArea).densityHistory.last._2 + ", " + totalInflow)
      sim.criticalAreas(fg.monitoredArea).regulatorIntegralAction = sim.criticalAreas(fg.monitoredArea).regulatorIntegralAction + (1.0 - sim.densityHistory.last._2)

      fg.flowRate = min(fg.width * 1.5, fg.width * (totalInflow / sim.controlDevices.flowGates.count(_.monitoredArea == fg.monitoredArea)))
      // when execution of release pedestrian takes place, if flow rate is 0 then the event will never happen. Hence manually insert one to restart flow gates.
      if (fg.flowRate > 0.0) {
        computeReleaseTimes(fg.flowRate, sim.evaluate_dt, Time(0.0), List()).foreach(t => sim.insertEventWithDelayNew(t)(new fg.ReleasePedestrian(sim)))
      }
    })
  }
}

