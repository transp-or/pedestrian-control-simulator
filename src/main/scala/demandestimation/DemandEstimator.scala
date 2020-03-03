package demandestimation

import hubmodel.demand.PublicTransportSchedule
import hubmodel.control.Measurement
import hubmodel.supply.graph.GraphContainer
import tools.Time

class DemandEstimator(infra: GraphContainer, prior: PublicTransportSchedule) {

  def estimateLinkFlows(measurement: Measurement, start: Time, end: Time) = {
    ???
  }

  val assignmentData = ???



}
