package trackingdataanalysis.pedtrack

/** All pedestrian classes should inherit from this trait. This simplifies the code requried for printing results as
  * not all different pedestrian specifications require different print methods. It also imposes the namig scheme.
  */
trait PedestrianTrait {
  def oZone: Int
  def dZone: Int
  def travelTime: Double
  def travelDistance: Double
  def meanVelocity: Double
  def entryTime: Double
  def exitTime: Double

  /** returns the OD pair as a tuple */
  def getODTuple: (Int, Int) = (oZone, dZone)

  /** return the OD pair as a Vector */
  def getODVec: Vector[Int] = Vector(oZone, dZone)

  /** needs to be specified for each implementation */
  override def toString: String
}