package trackingdataanalysis.pedtrack

/** Empirical pedestrian which extends [[trackingdataanalysis.pedtrack.PedestrianTrait]] with some extra members.
  *
  * @param ID              Unique ID (unique for a given day)
  * @param currentPosition initial position of the pedestrian
  * @param entryTime       time stamp of the fist occurence of the pedestrian
  */
class Pedestrian(val ID: Int, currentPosition: (Double, Double), val entryTime: Double) extends PedestrianTrait {

  val UUID: String = java.util.UUID.randomUUID.toString

  /* ---------- Members -----------*/
  var oCoords: (Double, Double) = (currentPosition._1.toDouble, currentPosition._2.toDouble)
  var oZone: Int = 0
  var dCoords: (Double, Double) = (0, 0)
  var dZone: Int = 0
  var travelTime: Double = 0
  var travelDistance: Double = 0
  var exitTime: Double = 0
  var meanVelocity: Double = 0
  val h_t: scala.collection.mutable.ArrayBuffer[Double] = scala.collection.mutable.ArrayBuffer(entryTime)
  val h_x: scala.collection.mutable.ArrayBuffer[Double] = scala.collection.mutable.ArrayBuffer(currentPosition._1)
  val h_y: scala.collection.mutable.ArrayBuffer[Double] = scala.collection.mutable.ArrayBuffer(currentPosition._2)
  val h_v: scala.collection.mutable.ArrayBuffer[(Double, Double)] = scala.collection.mutable.ArrayBuffer()

  def getTXYZipped = h_t.zip(h_x.zip(h_y))

  def getHistoryPosition: Vector[(Double, Vector2D)] = h_t.zip(h_x.zip(h_y).map(t => new Vector2D(t._1, t._2))).toVector

  /* ---------- Methods -----------*/

  /** moves the pedestrian and keeps track of some aggregate variables. The velocity is calculated using a backward
    * difference scheme. The exit time variable is used to compute the time increment between two observations.
    *
    * @param x new x position
    * @param y new y position
    * @param t new time
    */
  def updatePedestrian(x: Double, y: Double, t: Double): Unit = {
    travelDistance += math.sqrt(math.pow(x - h_x.last, 2) + math.pow(y - h_y.last, 2))
    h_v.append(((x - h_x.last) / (t - h_t.last), (y - h_y.last) / (t - h_t.last)))
    h_t.append(t)
    h_x.append(x)
    h_y.append(y)
    //h_vy.append((y-currentPosition._2)/(t-h_t.last))
    dCoords = (x, y)
    exitTime = t
    //currentPosition = (x, y)
  }

  def popHistory(): Unit = {
    h_t.drop(1)
    h_x.drop(1)
    h_y.drop(1)
    h_v.drop(1)
  }

  /** Prints a summary of the pedestrian as a string
    *
    * @return summary of the pedestrian as String
    */
  override def toString: String = {
    this.ID.toString + "," +
      this.oZone.toString + "," +
      this.oCoords + "," +
      this.dZone.toString + "," +
      this.entryTime.toString + "," +
      this.exitTime.toString + "," +
      this.travelTime.toString + "," +
      this.travelDistance.toString + "," +
      (this.travelDistance / this.travelTime).toString
  }

  def toJSON4HubModel: String = {
    "{ \"ID\":\"" + this.ID.toString + "\", \"O\":\"z_" + this.oZone.toString + "\", \"D\":\"z_" + this.dZone.toString + "\", \"entryTime\": " + this.entryTime + ", \"exitTime\":" + this.exitTime + "}"
  }
}