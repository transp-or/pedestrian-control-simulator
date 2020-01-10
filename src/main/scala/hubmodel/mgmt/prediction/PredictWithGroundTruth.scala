package hubmodel.mgmt.prediction

import hubmodel.DES.{Action, NOMADGraphSimulator, insertDemandIntoSimulator, mappingConceptualNode2GraphNodes}
import hubmodel.mgmt.state.{CurrentState, CurrentStateGroundTruth}
import hubmodel.ped.PedestrianNOMAD
import hubmodel.supply.graph.{GraphContainer, RouteGraph}
import tools.Time

class PredictWithGroundTruth[T <: PedestrianNOMAD](sim: NOMADGraphSimulator[T]) extends Prediction {

  val predictionHorizon: Time = Time(600)

  private lazy val predictionSimulator: NOMADGraphSimulator[T] = {

    val stateData: CurrentStateGroundTruth[T] = this.getStateData

    val graph: GraphContainer = sim.graph.deepCopy(stateData.controlDevices)

    val eventQueue = sim

    val simulator: NOMADGraphSimulator[T] = new NOMADGraphSimulator[T](
      sim.currentTime,
      sim.currentTime + predictionHorizon ,
      sf_dt = sim.sf_dt,
      route_dt = sim.route_dt,
      evaluate_dt = sim.evaluate_dt,
      rebuildTreeInterval = Some(sim.rebuildTreeInterval.get),
      spaceMicro = sim.spaceMicro,
      graph = graph,
      timeTable = sim.timeTable,
      stop2Vertex = sim.stop2Vertex,
      controlDevices = stateData.controlDevices,
      sim.logFullPedestrianHistory
    )

    // Insertion of the population
    stateData.population.foreach(simulator.insertInPopulation)

    simulator
  }

  /** Collect the state data from the ground truth simulator
    *
    * @return
    */
  protected def getStateData: CurrentStateGroundTruth[T] = {
    val pop = sim.population.map(p => p.copyState(sim.currentTime, false).asInstanceOf[T]).toVector
    new CurrentStateGroundTruth(pop, sim.controlDevices.deepCopyWithState(sim.currentTime, pop))
  }

  protected def predict(): Nothing = ???

}
