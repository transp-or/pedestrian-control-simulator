package hubmodel.demand.transit

import hubmodel.supply.{NodeParent, StopID_New, TrainID_New}
import tools.Time

case class Train(ID: TrainID_New, trainType: String, stop: StopID_New, arr: Option[Time], dep: Option[Time], capacity: Int) extends Vehicle
