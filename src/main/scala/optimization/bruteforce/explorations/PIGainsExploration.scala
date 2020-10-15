package optimization.bruteforce.explorations

import com.typesafe.config.Config
import hubmodel.DES.SimulationInputParameters
import hubmodel.demand.{PedestrianFlowFunction_New, PedestrianFlowPT_New, PedestrianFlow_New}
import optimization.bruteforce.parameters.ParameterModificationsPIGains

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

    (for (i <- BigDecimal(minP) to BigDecimal(maxP) by BigDecimal(interval); j <- BigDecimal(minI) to BigDecimal(maxI) by BigDecimal(interval); k <- 1 to config.getInt("sim.nb_runs")) yield {
        ParameterModificationsPIGains(i.toDouble, j.toDouble)
    }).toVector
  }


  override def getParameters(paramMods: ParameterModificationsPIGains): SimulationInputParameters = {
    defaultParameters.changePIGains(paramMods.P, paramMods.I)
  }

  override def getRunPrefix(paramMods: ParameterModificationsPIGains): String = {
    paramMods.P.toString + "_" + paramMods.I + "_params_"
  }

}
