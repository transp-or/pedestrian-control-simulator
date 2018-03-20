import java.io.{BufferedWriter, File, FileWriter}
import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.{floor, round}
import myscala.math.vector.{Vector2D, Vector3D}

/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {

  type NewBetterPosition2D = Vector2D
  type NewBetterDirection2D = Vector2D
  type NewBetterVelocity2D = Vector2D
  type NewBetterAcceleration2D = Vector2D
  type NewBetterForce2D = Vector2D

  def distance(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y), 0.5)
  def distance(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y) + (b.Z-a.Z)*(b.Z-a.Z), 0.5)



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


  class NewTime(val value: Double) extends AnyVal {

    def + (m: NewTime): NewTime = new NewTime(this.value + m.value)
    def addDouble (m: Double): NewTime = new NewTime(this.value + m)
    def - (m: NewTime): NewTime = new NewTime(this.value - m.value)
    def abs: NewTime = new NewTime(java.lang.Math.abs(this.value))

    def asReadable: String = {
      val hours: Int = floor(value / 3600.0).toInt
      val minutes: Int = floor((value - hours * 3600) / 60.0).toInt
      val seconds: Double = floor(value - hours * 3600 - minutes * 60)
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }

    def asVisioSafe: String = {
      val h: Int = floor(this.value / 3600).toInt
      val min: Int = floor((this.value - h * 3600) / 60).toInt
      val s: Int = floor(this.value - 3600 * h - 60 * min).toInt
      val ms: Int = round(1000 * (this.value - 3600 * h - 60 * min - s)).toInt
      h.toString + "," + min.toString + "," + s.toString + "," + ms.toString
    }

    override def toString: String = value.toString

  }

  object NewTime {
    def apply(value: Double): NewTime = new NewTime(value)

    def fromDouble(v: Double): NewTime = {new NewTime(v)}

    implicit def orderingByValue: Ordering[NewTime] = {
      Ordering.by(t => t.value)
    }
  }

  object NewTimeNumeric extends Ordering[NewTime] {
    def compare(x: NewTime, y: NewTime): Int = x.value compare y.value
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
  final case class VertexRectangle(name: String, A: NewBetterPosition2D, B: NewBetterPosition2D, C: NewBetterPosition2D, D: NewBetterPosition2D) {

    // unique identifier
    val ID: String = generateUUID

    // center of the rectangle
    val center: NewBetterPosition2D = A + (B - A)*0.5 + (D - A)*0.5

    // area of the associated zone
    val area: Double = (B - A).norm * (D - A).norm

    /** Is the point inside the vertex ?
      *
      * @param pos [[NewBetterPosition2D]] to check
      * @return boolean indicating if the point is inside the vertex.
      */
    def isInside(pos: NewBetterPosition2D): Boolean = {
      val AB: NewBetterPosition2D = B - A
      val BC: NewBetterPosition2D = C - B
      val AP: NewBetterPosition2D = pos - A
      val BP: NewBetterPosition2D = pos - B
      if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
      else false
    }

    def uniformSamplePointInside: NewBetterPosition2D = {
      Vector2D(ThreadLocalRandom.current.nextDouble(A.X+0.1, B.X-0.1), ThreadLocalRandom.current.nextDouble(A.Y+0.1, D.Y-0.1))
    }

    /** Equality based on the ID and not the positions
      *
      * @param other [[VertexRectangle]] to which we want to compare to
      * @return
      */
    def equalsID(other: Any): Boolean = {
      other match {
        case that: VertexRectangle => this.ID == that.ID
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
        case that: VertexRectangle => this.A == that.A && this.B == that.B && this.C == that.C && this.D == that.D
        case _ => false
      }

    /** Definition of equality.
      *
      * @return Int representing the object
      */
    override def hashCode: Int = {
      (this.A, this.B, this.C, this.D).##
    }

    override def toString: String = this.name

    def nameCompare(n: String): Boolean = this.name == n
  }


  /** Function to check whether a [[hubmodel.Position]] is inside a [[hubmodel.VertexRectangle]]. The default Vertex is a
    * plain rectangle, and hence this function checks whether the point is inside the rectangle. For more sophisticated
    * shapes, this function must be overriden.
    *
    * @param v   vertex
    * @param pos position ot check
    * @return boolean indicating if the point is inside the vertex
    */
  def isInVertex(v: VertexRectangle)(pos: NewBetterPosition2D): Boolean = {
    val AB: NewBetterPosition2D = v.B - v.A
    val BC: NewBetterPosition2D = v.C - v.B
    val AP: NewBetterPosition2D = pos - v.A
    val BP: NewBetterPosition2D = pos - v.B
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
  /*def timeBlock[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + " seconds")
    result
  }*/

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

  def writePopulationTrajectories(population: Iterable[PedestrianTrait], file: String): Unit = {
    val f = new File(file)
    val bw = new BufferedWriter(new FileWriter(f))
    var counter: Int = 0
    val totalPeds: Int = population.size
    for (p <- population) {
      counter += 1
      print(counter + "/" + totalPeds + " pedestrians processed\r")
      bw.write(p.toVisioSafeFormat().stripLineEnd)
      bw.write("\n")
    }
    bw.close()
  }
}
