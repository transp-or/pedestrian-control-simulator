package hubmodel.DES

import hubmodel.control.{ControlDevices, MeasurementError}
import hubmodel.demand.PublicTransportSchedule
import hubmodel.supply.continuous.ContinuousSpace
import hubmodel.supply.graph.{GraphContainer, Stop2Vertex}
import tools.Time

/** Container for all the parameters passed to the [[NOMADGraphSimulator]] pedestrian simulation.
  *
  * @param startTime start time of the simulation
  * @param endTime end time of the simulation
  * @param motionModelUpdateInterval interval for updating the pedestrian positions
  * @param updateRoutesInterval interval for updating the pedestrian's routes
  * @param spaceMicro space model used by the motion model
  * @param graph graph model used by the route choice model
  * @param stop2Vertex mapping between stops and vertices
  * @param controlDevices collection of control devices used in the simulation
  */
class SimulationInputParameters(var startTime: Time, var endTime: Time, val motionModelUpdateInterval: Time, val updateRoutesInterval: Time, val spaceMicro: ContinuousSpace, val graph: GraphContainer, val stop2Vertex: Stop2Vertex, val controlDevices: ControlDevices, val predictionParameters: PredictionInputParameters, val location: String, val setup: String, val dir: String = "", val measurementErrors: Vector[MeasurementError] = Vector()) {

  // Interval at which the density inside the monitored areas is computed
  var trackDensityInterval: Option[Time] = None

  // Interval at which the flow counters are reset. Basically the interval length of the flow counters
  var resetFlowCountersInterval: Option[Time] = None

  // Interval at which the state variables are computed
  var stateEvaluationInterval: Option[Time] = None

  // Interval at which the tree used for searching neighbour pedestrians is updated.
  var rebuildTreeInterval: Option[Time] = None

  // Timetable storing the arrival and departures of public transport vehicles
  var timeTable: Option[PublicTransportSchedule] = None

  // Store the full trajectories of pedestrians ? Default is false.
  var logFullPedestrianHistory: Boolean = false


  /** Creates a copy of the simulation parameters but uses the graph and control devices passed as arguments.
    * This is important as each different simulation must use a deep copy of the devices and graph.
    *
    * @param g graph to use
    * @param devices control devices to use
    * @return
    */
  def deepCopy(g: GraphContainer, devices: ControlDevices, motionUpdate: Option[Time]): SimulationInputParameters = {
    val params: SimulationInputParameters = new SimulationInputParameters(this.startTime, this.endTime, motionUpdate.getOrElse(this.motionModelUpdateInterval), this.updateRoutesInterval, this.spaceMicro, g, this.stop2Vertex, devices, predictionParameters, this.location, this.setup, this.dir)

    params.resetFlowCountersInterval = this.resetFlowCountersInterval
    params.trackDensityInterval = this.trackDensityInterval
    params.stateEvaluationInterval = this.stateEvaluationInterval
    params.rebuildTreeInterval = this.rebuildTreeInterval
    params.timeTable = this.timeTable
    params.logFullPedestrianHistory = this.logFullPedestrianHistory

    params
  }


  def changePIGains(P: Double, I: Double): SimulationInputParameters = {
    val params: SimulationInputParameters = new SimulationInputParameters(this.startTime, this.endTime, this.motionModelUpdateInterval, this.updateRoutesInterval, this.spaceMicro, this.graph, this.stop2Vertex, this.controlDevices.deepCopyModifyMovingWalkways(P, I, this.graph, this.controlDevices.flowLines, this.controlDevices.monitoredAreas.toVector), this.predictionParameters, this.location, this.setup, this.dir)

    params.resetFlowCountersInterval = this.resetFlowCountersInterval
    params.trackDensityInterval = this.trackDensityInterval
    params.stateEvaluationInterval = this.stateEvaluationInterval
    params.rebuildTreeInterval = this.rebuildTreeInterval
    params.timeTable = this.timeTable
    params.logFullPedestrianHistory = this.logFullPedestrianHistory

    params
  }
}
