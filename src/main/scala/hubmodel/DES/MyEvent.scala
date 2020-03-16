package hubmodel.DES

import tools.Time
import tools.TimeNumeric.mkOrderingOps

/** An Action becomes an Event when a time is associated.
  *
  * @param t      time at which the action must be performed
  * @param action the action itself (class with execute method)
  */
class MyEvent(val t: Time, val action: Action) extends Ordered[MyEvent] {

  // return 0 if the same, negative if this < that, positive if this > that (in terms of priority)
  override def compare(that: MyEvent): Int = {
    if (this.t > that.t) -1 // this.t is larger than that.t, that should be executed before this, hence that has higher priority
    else if (this.t < that.t) 1 // this.t is smaller than that.t, this should be executed before that, hence this has higher priority
    else 0
  }


  private var _skip: Boolean = false

  def skip: Boolean = this._skip

  def setSkipTrue(): Unit = {this._skip = true}

  /**
    * Writes the event as string with the execution time.
    *
    * @return string with the name of the event and the execution time.
    */
  override def toString: String = action.toString + " @ " + t
}