package trackingdataanalysis.pedtrack

import breeze.linalg.DenseVector
import hubmodel.generateUUID

package visiosafe {

  import myscala.math.linalg.areaFrom2DVectors
  import myscala.math.vector.Vector2D
  import trackingdataanalysis.pedtrack.io.InflowType

  /* ----------------------------------------------------------------------------------
                                 DATA CLASSES
   -----------------------------------------------------------------------------------*/

  /** A zone is built from 4 corners and is rectangular. The input array should be ordered as follows
    *
    * zone_id, zone_type, x_bl, y_bl, x_br, y_br, x_tr, y_tr, x_tl, y_tl
    *
    * where :
    *
    * - bl stands for bottom left
    *
    * - br stands for bottom right
    *
    * - tr stands for top righ
    *
    * - tl stands for top left
    *
    * @param data specification of the four corners, type and ID
    */
  class Zone(data: Array[Double]) {

    /* ---------- Members -----------*/
    val ID: Int = data(0).toInt
    val zType: Int = data(1).toInt
    val A: breeze.linalg.DenseVector[Double] = DenseVector(data(2), data(3))
    val B: breeze.linalg.DenseVector[Double] = DenseVector(data(4), data(5))
    val C: breeze.linalg.DenseVector[Double] = DenseVector(data(6), data(7))
    val D: breeze.linalg.DenseVector[Double] = DenseVector(data(8), data(9))

    val area: Double = (B(0) - A(0)) * (D(1) - A(1))
  }

  class NewZone(val name: String, val A: Position, val B: Position, val C: Position, val D: Position) {

    def this(name: String, A: NewPosition2D, B: NewPosition2D, C: NewPosition2D, D: NewPosition2D) {
      this(name, DenseVector(A._1, A._2), DenseVector(B._1, B._2), DenseVector(C._1, C._2), DenseVector(D._1, D._2))
    }

    // unique identifier
    val ID: String = generateUUID

    // center of the rectangle
    val center: Position = A + 0.5 * (B - A) + 0.5 * (D - A)

    // area of the associated zone
    val area: Double = areaFrom2DVectors(B - A, D - A)

    val sides: Vector[(NewPosition2D, NewPosition2D)] = Vector( ((A(0), A(1)), (B(0), B(1))), ((B(0), B(1)), (C(0), C(1))), ((C(0), C(1)), (D(0), D(1))), ((D(0), D(1)), (A(0), A(1))))

    /** Is the point inside the vertex ?
      *
      * @param pos [[Position]] to check
      * @return boolean indicating if the point is inside the vertex.
      */
    def isInside(pos: Position): Boolean = {
      val AB: DenseVector[Double] = B - A
      val BC: DenseVector[Double] = C - B
      val AP: DenseVector[Double] = pos - A
      val BP: DenseVector[Double] = pos - B
      if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
      else false
    }

    def isInside(pos: NewPosition2D): Boolean = isInside(DenseVector(pos._1, pos._2))

    /** Equality based on the ID and not the positions
      *
      * @param other [[NewZone]] to which we want to compare to
      * @return
      */
    def equalsID(other: Any): Boolean = {
      other match {
        case that: NewZone => this.ID == that.ID
        case _ => false
      }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[NewZone]

    /** Checks whether another object equals this one by comparing the positions associated to the vertex
      *
      * @param other another object to test equality for
      * @return boolean indicating if the two objects are the same
      */
    override def equals(other: Any): Boolean =
      other match {
        case that: NewZone => that.canEqual(this) && this.A == that.A && this.B == that.B && this.C == that.C && this.D == that.D
        case _ => false
      }

    /** Definition of equality.
      *
      * @return Int representing the object
      */
    override def hashCode: Int = {
      (this.A, this.B, this.C, this.D).##
    }

    override def toString: String = "(" + this.A.toString() + ", " + this.B.toString() + ", " + this.C.toString() + ", " + this.D.toString() + ")"
  }



    final class FlowLineDA(val name: String, val line: (NewPosition2D, NewPosition2D), val inflowType: InflowType) extends
      hubmodel.mgmt.flowsep.FlowLine(Vector2D(line._1._1, line._1._2), Vector2D(line._2._1, line._2._2), inflowType) {

      // 1 => from right to left
      // 0 => no side change
      // -1 => from left to right
      def crossesLine(currentPos: myscala.math.vector.Vector2D, previousPos: myscala.math.vector.Vector2D): Int = {
        if (this.nearRegion.isInside(currentPos)) {
          // https://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line/1560510#1560510
          if ( math.signum((end.X-start.X) * (currentPos.Y - start.Y) - (end.Y - start.Y)*(currentPos.X - start.X)) ==
            math.signum((end.X-start.X) * (previousPos.Y - start.Y) - (end.Y - start.Y)*(previousPos.X - start.X)) ) 0
          else if ( math.signum((end.X-start.X) * (currentPos.Y - start.Y) - (end.Y - start.Y)*(currentPos.X - start.X)) == 1 &&
            math.signum((end.X-start.X) * (previousPos.Y - start.Y) - (end.Y - start.Y)*(previousPos.X - start.X)) != 1) 1
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
