package hubmodel.DES

import com.typesafe.scalalogging.LazyLogging

/**
  * Parent to all actions which will appear in the DES.
  * The only method which is mandatory is "execute". To give more control over what data is passed to the events,
  * the Actions which inherent from [[Action]] should take as arguments the simulation.
  */
abstract class Action extends LazyLogging {


  /** Relative priority between different classes. The higher the value, the higher the priority. This means
    * that an [[Action]] with a higher priority will be executed before another action with lower priority.
    *
    * This should be used carefully as wrong priority can lead to problematic situations.
    */
  val priority: Int = 0

  /** Method which is called to do stuff. This method must be overriden for all subclasses of [[Action]].
    * The method will modifiy some elements of the simulation.
    */
  def execute(): Any

  override def toString: String = this.getClass.getSimpleName

}
