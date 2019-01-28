package trackingdataanalysis.pedtrack

package simulation {

  //import AggDisagg.Pedestrian

  /**
    * Created by nicholas on 16/03/17.
    */

  //case class Pedestrian(oZone: Int, dZone: Int, entryTime: Double, meanVelocity: Double, travelTime: Double = Double.NaN, exitTime: Double = Double.NaN, travelDistance: Double = Double.NaN)

  class PedestrianSim(var oZone: Int,
                      var dZone: Int,
                      var meanVelocity: Double,
                      var travelTime: Double,
                      var travelDistance: Double,
                      var entryTime: Double,
                      var exitTime: Double) extends PedestrianTrait {

    def this(oZone: Int, dZone: Int, freeFlowVel: Double) {
      this(oZone, dZone, freeFlowVel, Double.NaN, Double.NaN, Double.NaN, Double.NaN)
    }

    def this(oZone: Int, dZone: Int, freeFlowVel: Double, entryTime: Double) {
      this(oZone, dZone, freeFlowVel, Double.NaN, Double.NaN, entryTime, Double.NaN)
    }

    override def toString: String = {
      this.oZone.toString + "," + this.dZone.toString + "," + this.travelTime.toString + "," + this.travelDistance.toString
    }
  }

}

