package hubmodel.demand.transit

import hubmodel.Time
import hubmodel.supply.{ODIDOld, StopID_New, TrainID_New}

class Train(ID: TrainID_New, trainType: String, track: StopID_New, arr: Option[Time], dep: Option[Time], capacity: Int) extends Vehicle(ID, trainType, track, arr, dep, capacity) {

  //val IDNew: TrainID_New = TrainID_New(ID, ID)

  override def toString: ODIDOld = {
    arr match {
      case Some(str) => {
        dep match {
          case Some(str2) => ID + ", track=" + track + ", arrival @ " + str + ", departure @ " + str2 + " with capacity=" + capacity
          case None => ID + ", track=" + track + ", arrival @ " + str + ", no departure " + " with capacity=" + capacity
        }
      }
      case None => {
        dep match {
          case Some(str2) => ID + ", track=" + track + ", no arrival, " + "departure @ " + str2 + " with capacity=" + capacity
          case None => ID + ", track=" + track + ", no arrival, no departure " + " with capacity=" + capacity
        }
      }
    }
  }

}
