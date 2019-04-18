package optimization.simulation


import SimulatedAnnealing.SAProblem
import SimulatedAnnealing._MinFunction.MinFunction
import java.util
import java.util.List
import java.util.stream.Collectors

import SimulatedAnnealing.Factories.SAProblemsAbstractFactory
import com.typesafe.config.Config


class SingleGateOptimizationFactory(config: Config, ID: String) extends SAProblemsAbstractFactory { // MinFunction est definie que par un seul double

  def createSAProblem(objects: util.List[java.lang.Object]): SAProblem = {

    new SingleGateOptimisation(config, ID, objects.asInstanceOf[java.util.ArrayList[java.lang.Double]])

  }

}
