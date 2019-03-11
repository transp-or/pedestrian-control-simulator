package hubmodel.results

import hubmodel.ped.PedestrianSim
import hubmodel.tools.cells.DensityMeasuredArea
import hubmodel.DES.PedestrianDES

/**
  * Container for storing all the results from the simulation. This should be returned from the run method of the
  * [[hubmodel.DES.NOMADGraphSimulator]] simulator. The exit code indicates if the simulation was terminated
  * prematurely for some reason.
  *
  * @param exitCode 0 if normal termination.
  * @param completedPeds list of pedestrians who finished their journey.
  * @param uncompletedPeds list of pedestrians who didn't finished their journey, but started it.
  * @param densityZones list of zones where the density was computed.
  */
case class ResultsContainerFromSimulation(exitCode: Int, completedPeds: Vector[PedestrianSim], uncompletedPeds: Vector[PedestrianSim], densityZones: Map[String, DensityMeasuredArea])

