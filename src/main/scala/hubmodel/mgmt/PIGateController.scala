package hubmodel.mgmt

import breeze.linalg.{max, min}
import hubmodel.DES.{Action, SFGraphSimulator}
import hubmodel.NewTimeNumeric.mkOrderingOps
import hubmodel.Time
import hubmodel.supply.graph.FlowGate

class PIGateController(sim: SFGraphSimulator) extends Action {

  def allocateSupply(totalInflow: Double, gates: Vector[String]): Map[String, Double] = {
    gates.map(g => g -> totalInflow / gates.size).toMap
  }

  def computeReleaseTimes(rate: Double, maxTime: Time, currentTime: Time, acc: List[Time]): List[Time] = {
    if (currentTime >= maxTime) acc
    else computeReleaseTimes(rate, maxTime, currentTime.addDouble(1.0 / rate), currentTime.addDouble(1.0 / rate) :: acc)
  }

  override def execute(): Unit = {
    sim.eventLogger.trace("time: " + sim.currentTime + ": updating flow gates using PI controller")
    val totalInflow: Double = max(0.1, min(3.0, sim.inflowHistory.last._2 - 0.6 * (sim.densityHistory.last._2 - sim.densityHistory.dropRight(1).last._2) + 0.32 * (1.0 - sim.densityHistory.last._2)))
    //println("PI data @ "+ sim.currentTime + ", " + sim.densityHistory.last._2 + "," + (sim.densityHistory.last._2 - sim.densityHistory.dropRight(1).last._2) + ", " + (0.8 - sim.densityHistory.last._2) + ", " + totalInflow)

    sim.inflowHistory.append((sim.currentTime, totalInflow))
    val flowGatesTotalWidth: Double = sim.controlDevices.flowGates.foldLeft(0.0) { (o: Double, n: FlowGate) => o + n.width }

    //if (sim.densityHistory.last._2 > 0.1) {
    //sim.inflowHistory.append((sim.currentTime,totalInflow))
    sim.controlDevices.flowGates.foreach(fg => {
      fg.flowRate = min(fg.width * 1.5, fg.width * (totalInflow / sim.controlDevices.flowGates.size))
      // when execution of release pedestrian takes place, if flow rate is 0 then the event will never happen. Hence manually insert one to restart flow gates.
      if (fg.flowRate > 0.0) {
        computeReleaseTimes(fg.flowRate, sim.evaluate_dt, Time(0.0), List()).foreach(t => sim.insertEventWithDelayNew(t)(new fg.ReleasePedestrian(sim)))
      }
      /*else {
        sim.insertEventWithDelay(1.0 / min(fg.width * 1.5, fg.width * (totalInflow / sim.graph.flowGates.size)))(new fg.ReleasePedestrian(sim))
      }*/
    })

    /*sim.graph.flowGates.foreach(fg => {
      computeReleaseTimes(fg.flowRate, sim.evaluate_dt, 0.0, List()).foreach(t => sim.insertEventWithDelay(t)(new fg.ReleasePedestrian(sim)))
    })*/
    //println("total inflow=" + totalInflow + ", density=" + (sim.densityHistory.dropRight(1).last._2, sim.densityHistory.last._2) + " @ time=" + sim.currentTime)
    //println(sim.inflowHistory.last._2, sim.densityHistory.last._2, sim.densityHistory.dropRight(1).last._2)
    //} else {
    //val totalInflow: Double = sim.inflowHistory.last._2 - 8.0*(sim.densityHistory.last._2 - sim.densityHistory.dropRight(1).last._2) + 0.2*(1.5 - sim.densityHistory.last._2)
    //sim.graph.flowGates.foreach(fg => fg.flowRate = fg.width*1.5)
    //println("total inflow=" + totalInflow + ", density=" + (sim.densityHistory.dropRight(1).last._2, sim.densityHistory.last._2) + " @ time=" + sim.currentTime)
    //}
    //sim.graph.flowGates.foreach(fg => println(totalInflow + "," + sim.currentTime + "," + fg.flowRate))
    //println(sim.graph.flowGates.map(_.flowRate).mkString(", "))
  }
}

