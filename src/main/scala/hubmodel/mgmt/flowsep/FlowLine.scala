package hubmodel.mgmt.flowsep

import hubmodel.ped.{PedestrianTrait, Population}
import hubmodel.tools.cells.Rectangle
import hubmodel.{FLOW_LINE_REGION_EXTENSION, Position}

class FlowLine(val start: Position, val end: Position, controlled: Int = 0) {

  // area in which to consider pedestrians for crossing line
  val nearRegion: Rectangle = {

    new Rectangle(
      "nearRegion",
      start - (end - start).orthogonal * FLOW_LINE_REGION_EXTENSION,
      end - (end - start).orthogonal * FLOW_LINE_REGION_EXTENSION,
      end + (end - start).orthogonal * FLOW_LINE_REGION_EXTENSION,
      start + (end - start).orthogonal * FLOW_LINE_REGION_EXTENSION
    )
  }

  /**
    * Determines if a given pedestrian has changed sides during
    * @param ped pedestrian to check
    * @return boolean indicating if the ped did cross the line
    */
  def crossesLineRight2Left(ped: PedestrianTrait): Boolean = {
    this.nearRegion.isInside(ped.currentPosition) && {
      // https://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line/1560510#1560510
      if ( math.signum((end.X-start.X) * (ped.currentPosition.Y - start.Y) - (end.Y - start.Y)*(ped.currentPosition.X - start.X)) == 1 &&
        math.signum((end.X-start.X) * (ped.previousMajorPosition.Y - start.Y) - (end.Y - start.Y)*(ped.previousMajorPosition.X - start.X)) != 1 ) true
      else false
    }
  }

  /** Stores the number of pedestrians who crossed the line during the last interval */
  private val pedsCrossedInInterval: collection.mutable.Set[String] = collection.mutable.Set()

  /**
    * Adds all the pedestrians who crossed the line into [[pedsCrossedInInterval]]
    * @param pop
    */
  def collectPedestriansWhoCrossed(pop: Population): Unit = {
    pop.filter(this.crossesLineRight2Left).foreach(ped => this.pedsCrossedInInterval.add(ped.ID))
  }

  /**
    * Gets the number of pedestians who crossed the line (the size of the Set [[pedsCrossedInInterval]]
    * @return number of peds
    */
  def getPedestrianFlow: Int = this.pedsCrossedInInterval.size

  /**
    * Clears the collection of pedestrians stored in [[pedsCrossedInInterval]].
    * This should be called at the start of each new time interval of the state evaluation.
    */
  def reinitialize(): Unit = this.pedsCrossedInInterval.clear()

  override def clone(): FlowLine = new FlowLine(
    this.start, this.end
  )
}
