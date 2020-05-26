package hubmodel.ped

import hubmodel.Position
import hubmodel.ped.History.HistoryContainer
import tools.Time
import tools.cells.Vertex

/** Enforces the format for storing the trajectory of the pedestrian. This is used in the tracking data analysis
  * AND the simulation. This way, the same visualization tools can be used.
  *
  */
trait PedestrianTrajectory {

  /** Trajectory container for storing all positions of the pedestrian.
    *
    */
  protected val _historyPosition: Vector[(Time, HistoryContainer)]

  protected val _historyPositionUnsorted: collection.mutable.ArrayBuffer[(Time, HistoryContainer)]


  /** Getter method for the trajectory (position history).
    *
    * @return Pedestrian trajectory
    */
  def getHistoryPosition: Vector[(Time, HistoryContainer)] = _historyPosition.toVector


  /** Updates the trajectory at the current time
    *
    * @param t time to add the current position
    */
  def updatePositionHistory(t: Time, pos: Position): Unit


  protected val _accomplishedRoute: collection.mutable.ArrayBuffer[(Time, Vertex, Position)] = new collection.mutable.ArrayBuffer()

  def appendAccomplishedRoute(t: Time, v: Vertex ,p: Position): Unit = {this._accomplishedRoute.append((t, v, p))}

  def accomplishedRoute: Vector[(Time, Vertex, Position)] = this._accomplishedRoute.toVector
}



object HistoryOrdering extends Ordering[(Time, HistoryContainer)] {

  /** Sorting of two elements is based on the time, and nothing else.
    *
    * @param a first element to compare
    * @param b second element to compare to
    * @return Result of comparing this with operand that. returns x where x < 0 iff a < b x == 0 iff a == b x > 0 iff a > b
    */
  def compare(a: (Time, HistoryContainer), b:(Time, HistoryContainer)): Int = tools.TimeNumeric.compare(a._1 , b._1)
}
