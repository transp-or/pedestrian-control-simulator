import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.{floor, round}
import myscala.math.linalg.areaFrom2DVectors
import breeze.linalg.DenseVector

/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {

  trait PhysicalVector

  trait PhysicalVectorOps[A] {

    def + (that: A): A
    def - (that: A): A

    def + (d: Double): A
    def - (d: Double): A
    def * (d: Double): A
    def / (d: Double): A

    def norm: Double
    def distanceTo(that: A): Double
    def dot(that: A): Double
    def normalize: A
  }

  sealed class Vector2D(val X: Double, val Y: Double) extends PhysicalVector with PhysicalVectorOps[Vector2D] {

    def + (that: Vector2D): Vector2D = {new Vector2D(this.X + that.X, this.Y + that.Y)}
    def - (that: Vector2D): Vector2D = {new Vector2D(this.X - that.X, this.Y - that.Y)}

    def + (i: Double): Vector2D = {new Vector2D(this.X + i, this.Y + i)}
    def - (i: Double): Vector2D = {new Vector2D(this.X - i, this.Y - i)}
    def * (i: Double): Vector2D = {new Vector2D(this.X * i, this.Y * i)}
    def / (i: Double): Vector2D = {new Vector2D(this.X / i, this.Y / i)}

    def norm: Double = scala.math.pow(this.X*this.X + this.Y*this.Y, 0.5)
    def distanceTo(that: Vector2D): Double = scala.math.pow((that.X-this.X)*(that.X-this.X) + (that.Y-this.Y)*(that.Y-this.Y), 0.5)
    def dot(that: Vector2D): Double = this.X*that.X + this.Y*that.Y
    def normalize: Vector2D = if (this.norm == 0.0) {this} else {new Vector2D(this.X / this.norm, this.Y / this.norm)}

    override def toString: String = "(" + X + ", " + Y + ")"
  }

  final class ZeroVector2D extends Vector2D(0.0, 0.0)

  sealed class Vector3D(val X: Double, val Y: Double, val Z: Double) extends PhysicalVector with PhysicalVectorOps[Vector3D] {

    def + (that: Vector3D): Vector3D = {new Vector3D(this.X + that.X, this.Y + that.Y, this.Z + that.Z)}
    def - (that: Vector3D): Vector3D = {new Vector3D(this.X - that.X, this.Y - that.Y, this.Z - that.Z)}

    def + (i: Double): Vector3D = {new Vector3D(this.X + i, this.Y + i, this.Z + i)}
    def - (i: Double): Vector3D = {new Vector3D(this.X - i, this.Y - i, this.Z - i)}
    def * (i: Double): Vector3D = {new Vector3D(this.X * i, this.Y * i, this.Z * i)}
    def / (i: Double): Vector3D = {new Vector3D(this.X / i, this.Y / i, this.Z / i)}

    def distanceTo(that: Vector3D): Double = scala.math.pow((that.X-this.X)*(that.X-this.X) + (that.Y-this.Y)*(that.Y-this.Y) + (that.Z-this.Z)*(that.Z-this.Z), 0.5)
    def norm: Double = scala.math.pow(this.X*this.X + this.Y*this.Y + this.Z*this.Z, 0.5)
    def dot(that: Vector3D): Double = this.X*that.X + this.Y*that.Y + this.Z*that.Z
    def normalize: Vector3D = new Vector3D(this.X / this.norm, this.Y / this.norm, this.Z / this.norm)

    override def toString: String = "(" + X + ", " + Y + ", " + Z + ")"
  }

  final class ZeroVector3D extends Vector3D(0.0, 0.0, 0.0)

  type NewBetterPosition2D = Vector2D
  type NewBetterDirection2D = Vector2D
  type NewBetterVelocity2D = Vector2D
  type NewBetterAcceleration2D = Vector2D
  type NewBetterForce2D = Vector2D

  def distance(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y), 0.5)
  def distance(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y) + (b.Z-a.Z)*(b.Z-a.Z), 0.5)

  def normNew(a: Vector2D): Double = scala.math.pow(a.X*a.X + a.Y*a.Y, 0.5)
  def normNew3d(a: Vector3D): Double = scala.math.pow(a.X*a.X + a.Y*a.Y + a.Z*a.Z, 0.5)



  @deprecated
  type NewPosition2D = (Double, Double)

  @deprecated
  def norm(a: NewPosition2D, b: NewPosition2D): Double = scala.math.pow((b._1-a._1)*(b._1-a._1) + (b._2-a._2)*(b._2-a._2), 0.5)

  // Type representing a 2D position
  @deprecated
  type Position = breeze.linalg.DenseVector[Double]

  // Type representing a 2D direction
  @deprecated
  type Direction = breeze.linalg.DenseVector[Double]

  // Type representing a 2D velocity
  @deprecated
  type Velocity = breeze.linalg.DenseVector[Double]

  // Type representing a 2D acceleration
  @deprecated
  type Acceleration = breeze.linalg.DenseVector[Double]

  // Type representing a 2D force
  @deprecated
  type Force = breeze.linalg.DenseVector[Double]

  // Type for representing Time: in seconds !
  @deprecated
  type Time = Double

  class NewTime(val time: Double) extends AnyVal {

    def asReadable: String = {
      val hours: Int = floor(time / 3600.0).toInt
      val minutes: Int = floor((time - hours * 3600) / 60.0).toInt
      val seconds: Double = time - hours * 3600 - minutes * 60
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

  }

  /** Implicit conversion for printing the [[hubmodel.Time]] type. Covnerts the Double to readable
    * time for humans. Converts the seconds to hours, minutes and seconds of the day.
    *
    * @param t Time to convert
    */
  @deprecated
  implicit class timePrint(t: Time) {

    /** Prints the time as a String
      *
      * @return
      */
    def timePrint: String = t.toString

    /** Converts the number of seconds to readable human time.
      *
      * @return Time formatted as hh:mm:ss
      */
    def timeReadable: String = {
      val hours: Int = floor(t / 3600.0).toInt
      val minutes: Int = floor((t - hours * 3600) / 60.0).toInt
      val seconds: Double = t - hours * 3600 - minutes * 60
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }
  }

  /** Takes a [[hubmodel.Time]] as argument and converts it to a readable time.
    *
    * @param t [[hubmodel.Time]] to convert
    * @return formatted as hh::mm::ss
    */
  def timeReadable(t: Time): String = {
    val hours: Int = floor(t / 3600.0).toInt
    val minutes: Int = floor((t - hours * 3600) / 60.0).toInt
    val seconds: Double = t - hours * 3600 - minutes * 60
    hours.toString + ":" + minutes.toString + ":" + seconds.toString
  }

  /** Formats the [[hubmodel.Time]] as VisioSafe does.
    *
    * @param t [[hubmodel.Time]] to convert
    * @return formatted as h,m,s,ms
    */
  def time2VisioSafeTime(t: Time): String = {
    val sec = t
    val h: Int = floor(sec / 3600).toInt
    val min: Int = floor((sec - h * 3600) / 60).toInt
    val s: Int = floor(sec - 3600 * h - 60 * min).toInt
    val ms: Int = round(1000 * (sec - 3600 * h - 60 * min - s)).toInt
    h.toString + "," + min.toString + "," + s.toString + "," + ms.toString
  }

  /** Generation of a UUID. This can be used to generate unique identifiers for objects.
    *
    * @return UUID formatted as a String
    */
  def generateUUID: String = java.util.UUID.randomUUID.toString

  /** Representation of a vertex, or cell. This is used for representing vertices in the graph specification
    * and can be extend to manage any sort cell or zone. This specification is that of rectangles with vertical
    * and horizontal sides. For more advanced shapes, this class should be overriden. If this is done, the methods
    * for generating points inside the cells must also be overriden.
    *
    * @param name name for humans
    * @param A    bottom left
    * @param B    bottom right
    * @param C    top right
    * @param D    top left
    */
  final case class VertexCell(name: String, A: Position, B: Position, C: Position, D: Position) {

    // unique identifier
    val ID: String = generateUUID

    // center of the rectangle
    val center: Position = A + 0.5 * (B - A) + 0.5 * (D - A)

    // area of the associated zone
    val area: Double = areaFrom2DVectors(B - A, D - A)

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

    def uniformSamplePointInside: NewPosition2D = {
      (ThreadLocalRandom.current.nextDouble(A(0), B(0)), ThreadLocalRandom.current.nextDouble(A(1), D(1)))
    }

    /** Equality based on the ID and not the positions
      *
      * @param other [[VertexCell]] to which we want to compare to
      * @return
      */
    def equalsID(other: Any): Boolean = {
      other match {
        case that: VertexCell => this.ID == that.ID
        case _ => false
      }
    }

    /** Checks whether another object equals this one by comparing the positions associated to the vertex
      *
      * @param other another object to test equality for
      * @return boolean indicating if the two objects are the same
      */
    override def equals(other: Any): Boolean =
      other match {
        case that: VertexCell => this.A == that.A && this.B == that.B && this.C == that.C && this.D == that.D
        case _ => false
      }

    /** Definition of equality.
      *
      * @return Int representing the object
      */
    override def hashCode: Int = {
      (this.A, this.B, this.C, this.D).##
    }
  }


  /** Function to check whether a [[hubmodel.Position]] is inside a [[hubmodel.VertexCell]]. The default Vertex is a
    * plain rectangle, and hence this function checks whether the point is inside the rectangle. For more sophisticated
    * shapes, this function must be overriden.
    *
    * @param v   vertex
    * @param pos position ot check
    * @return boolean indicating if the point is inside the vertex
    */
  def isInVertex(v: VertexCell)(pos: Position): Boolean = {
    val AB: DenseVector[Double] = v.B - v.A
    val BC: DenseVector[Double] = v.C - v.B
    val AP: DenseVector[Double] = pos - v.A
    val BP: DenseVector[Double] = pos - v.B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

  def isInVertexNew(v: VertexCell)(pos: NewBetterPosition2D): Boolean = {
    val AB: DenseVector[Double] = v.B - v.A
    val BC: DenseVector[Double] = v.C - v.B
    val AP: DenseVector[Double] = DenseVector(pos.X, pos.Y) - v.A
    val BP: DenseVector[Double] = DenseVector(pos.X, pos.Y) - v.B
    if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
    else false
  }

  /** Prints the time taken to execute a block of code. This function is used as
    * {{{
    * val g = timeBlock {
    *   buildGraph(conn3.head, conn3.tail, List())
    * }}}
    *
    * @param block code to evaluate
    * @tparam R return type of that code
    * @return returns the same object as the original block would of done
    */
  def timeBlock[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + " seconds")
    result
  }

  /** Function on which to place a breakpoint for debugging. One way to use this function is the following:
    * {{{
    * val density: Double = 1.2
    * if (density > 1.0) {
    *   enterDebugMethod
    * }
    * }}}
    * This way the debuger will stop when the condition is met.
    */
  def enterDebugMethod: Unit = {
    println("called debug function")
  }
}
