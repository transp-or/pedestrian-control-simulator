package optimization.DSA


import java.util

import SimulatedAnnealing.Factories.SAProblemsAbstractFactory
import SimulatedAnnealing.SAProblem
import com.typesafe.config.Config


class SingleGateOptimizationFactory(config: Config, ID: String) extends SAProblemsAbstractFactory { // MinFunction est definie que par un seul double

  def createSAProblem(objects: util.List[java.lang.Object]): SAProblem = {

    new SingleGateOptimisation(config, ID, objects.asInstanceOf[java.util.ArrayList[java.lang.Double]])

  }

}
