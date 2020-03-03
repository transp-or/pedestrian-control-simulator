package trackingdataanalysis.pedtrack

package visiosafe {

  import myscala.math.vector.Vector2D
  import trackingdataanalysis.pedtrack.io.InflowType

  /* ----------------------------------------------------------------------------------
                                 DATA CLASSES
   -----------------------------------------------------------------------------------*/

  final class FlowLineDA(val name: String, val line: (NewPosition2D, NewPosition2D), val inflowType: InflowType) extends
    hubmodel.control.flowsep.FlowLine(Vector2D(line._1._1, line._1._2), Vector2D(line._2._1, line._2._2), inflowType) {

    // 1 => from right to left
    // 0 => no side change
    // -1 => from left to right
    def crossesLine(currentPos: myscala.math.vector.Vector2D, previousPos: myscala.math.vector.Vector2D): Int = {
      if (this.nearRegion.isInside(currentPos)) {
        // https://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line/1560510#1560510
        if (math.signum((end.X - start.X) * (currentPos.Y - start.Y) - (end.Y - start.Y) * (currentPos.X - start.X)) ==
          math.signum((end.X - start.X) * (previousPos.Y - start.Y) - (end.Y - start.Y) * (previousPos.X - start.X))) 0
        else if (math.signum((end.X - start.X) * (currentPos.Y - start.Y) - (end.Y - start.Y) * (currentPos.X - start.X)) == 1 &&
          math.signum((end.X - start.X) * (previousPos.Y - start.Y) - (end.Y - start.Y) * (previousPos.X - start.X)) != 1) 1
        else -1
      }
      else 0
    }

    /** Checks whether we are allowed to compare this object to another
      *
      * @param other
      * @return
      */
    def canEqual(other: Any): Boolean = other.isInstanceOf[FlowLineDA]


    /** Checks whether another object equals this one by comparing the positions associated to the vertex
      *
      * @param other another object to test equality for
      * @return boolean indicating if the two objects are the same
      */
    override def equals(other: Any): Boolean =
      other match {
        case that: FlowLineDA => super.equals() && that.canEqual(this) && this.line._1 == that.line._1 && this.line._2 == that.line._2
        case _ => false
      }

    /** Definition of equality.
      *
      * @return Int representing the object
      */
    override def hashCode: Int = {
      (super.hashCode, this.line._1, this.line._2).##
    }
  }

}
