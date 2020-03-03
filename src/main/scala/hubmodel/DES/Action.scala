package hubmodel.DES

import hubmodel.StrictLogging

/**
  * Parent to all actions which will appear in the DES.
  * The only method which is mandatory is "execute". To give more control over what data is passed to the events,
  * the Actions which inherent from [[Action]] should take as arguments the simulation.
  */
trait Action extends StrictLogging {


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

  type A <: Action

  /** Creates a deep copy of this action. This is necessary when doing predictions with the ground truth simulator.
    *
    * @return this action copied
    */
  def deepCopy(simulator: PedestrianPrediction): Option[A]

  override def toString: String = this.getClass.getSimpleName

}
