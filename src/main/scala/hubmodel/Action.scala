package hubmodel


/**
  * Parent to all actions which will appear in the DES.
  * The only method which is mandatory is "execute". To give more control over what data is passed to the events,
  * the Actions which inherent from [[Action]] should take as arguments the simulation.
  */
trait Action {

  /** Method which is called to do stuff. This method must be overriden for all subclasses of [[Action]].
    * The method will modifiy some elements of the simulation.
    */
  def execute(): Unit
}
