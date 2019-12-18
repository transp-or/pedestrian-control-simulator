package hubmodel.demand.transit

import java.util.concurrent.ThreadLocalRandom

import hubmodel.supply.{ODIDOld, StopID_New, TrainID_New}
import tools.Time

case class TrainWithExchangeVolumes( ID: TrainID_New,
                                     trainType: String,
                                     stop: StopID_New,
                                     arr: Option[Time],
                                     dep: Option[Time],
                                     capacity: Int,
                                     track: Int,
                                     carriages: Int,
                                     private val arrHOP: Option[Double],
                                     private val depHOP: Option[Double],
                                     private val arrFRASY: Option[Double],
                                     private val depFRASY: Option[Double]) extends Vehicle {


  private val FQ_std: Double = 0.2  // Relative standard deviation of FQ counts


  val disembarkingPassengers: Option[Double] = {
    if (arr.nonEmpty && arrHOP.nonEmpty){
      arrHOP.map(v => v + /*ThreadLocalRandom.current().nextGaussian() **/ v * FQ_std)
    } else if (arr.nonEmpty && arrFRASY.nonEmpty) {
      arrFRASY.map(v => v +/* ThreadLocalRandom.current().nextGaussian() **/ v * FQ_std)
    } else {
      None
    }
  }

  val boardingPassengers: Option[Double] = {
    if (dep.nonEmpty && depHOP.nonEmpty){
      depHOP.map(v => v /*+ ThreadLocalRandom.current().nextGaussian() *  v * FQ_std*/)
    } else if (dep.nonEmpty && depFRASY.nonEmpty) {
      depFRASY.map(v => v /*+ ThreadLocalRandom.current().nextGaussian() *  v * FQ_std*/)
    } else {
      None
    }
  }

 //val IDNew: TrainID_New = TrainID_New(ID, ID)

  override def toString: ODIDOld = {
    arr match {
      case Some(str) => {
        dep match {
          case Some(str2) => "Train(" + ID + ", track=" + track + ", arrival @ " + str + ", departure @ " + str2 + " with capacity=" + capacity + ")"
          case None => ID + "Train(" + ID  + ", track=" + track + ", arrival @ " + str + ", no departure " + " with capacity=" + capacity + ")"
        }
      }
      case None => {
        dep match {
          case Some(str2) => "Train(" + ID + ", track=" + track + ", no arrival, " + "departure @ " + str2 + " with capacity=" + capacity + ")"
          case None => "Train(" + ID + ", track=" + track + ", no arrival, no departure " + " with capacity=" + capacity + ")"
        }
      }
    }
  }

}
