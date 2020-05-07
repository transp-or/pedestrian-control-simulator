package hubmodel.DES

import tools.Time

/** Container for all the parameters related to the predictions.
  *
  * @param horizon prediction horizon
  * @param updateInterval interval at which the prediction is updated
  * @param decisionVariableLength duration of the decision variables
  * @param densityUpdateInterval interval at which the density is computed
  * @param replications number of replications to perform
  */
class PredictionInputParameters(val horizon: Time,
                                val updateInterval: Time,
                                val decisionVariableLength: Time,
                                val densityUpdateInterval: Time,
                                val replications: Int,
                                val threads: Int)
