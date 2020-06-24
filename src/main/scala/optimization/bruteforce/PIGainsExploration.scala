package optimization.bruteforce

import com.typesafe.config.Config
import hubmodel.DES.{SimulationInputParameters, createRunWriteSimulation, getDisaggregateFlows}
import hubmodel.control.{ControlDevicePolicy, ControlDevices}
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.immutable.{ParSeq, ParVector}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport

class PIGainsExploration(minP: Double, maxP:Double, minI: Double, maxI: Double, interval: Double, c: Config) extends GridSearchNew[ParameterModificationsPIGains](c) {

  /** Not used in this case. Shouldn't be abstract method of grid search.
    *
    * @param paramMods
    * @return
    */
  def getFlowMods(paramMods: ParameterModificationsPIGains): (Iterable[PedestrianFlow_New], Iterable[PedestrianFlowPT_New], Iterable[PedestrianFlowFunction_New]) = {
    (Vector(), Vector(), Vector())
  }

  val simulationRunsParameters: Vector[ParameterModificationsPIGains] = {

    (for (i <- BigDecimal(minP) to BigDecimal(maxP) by BigDecimal(interval); j <- BigDecimal(minP) to BigDecimal(maxP) by BigDecimal(interval); k <- 1 to config.getInt("sim.nb_runs")) yield {
        ParameterModificationsPIGains(i.toDouble, j.toDouble)
    }).toVector
  }


  override def getParameters(paramMods: ParameterModificationsPIGains): SimulationInputParameters = {
    val parameters : SimulationInputParameters = defaultParameters
    val newDevices : ControlDevices = parameters.controlDevices.deepCopyModifyMovingWalkways(paramMods.P, paramMods.I, parameters.graph, parameters.controlDevices.flowLines, parameters.controlDevices.monitoredAreas.toVector)
    parameters.deepCopy(parameters.graph, newDevices)
  }

  override def getRunPrefix(paramMods: ParameterModificationsPIGains): String = {
    paramMods.P.toString + "_" + paramMods.I + "_params_"
  }

}
