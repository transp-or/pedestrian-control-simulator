import java.io.{BufferedWriter, File, FileWriter}
import java.util.concurrent.ThreadLocalRandom

import breeze.numerics.{floor, round}
import hubmodel.ped.PedestrianTrait
import myscala.math.vector.{Vector2D, Vector3D}

/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {

  type Position = Vector2D
  type Direction = Vector2D
  type Velocity = Vector2D
  type Acceleration = Vector2D
  type Force = Vector2D

  def distance(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y), 0.5)
  def distance(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y) + (b.Z-a.Z)*(b.Z-a.Z), 0.5)


  class Time(val value: Double) extends AnyVal {

    def + (m: Time): Time = new Time(this.value + m.value)
    def addDouble (m: Double): Time = new Time(this.value + m)
    def - (m: Time): Time = new Time(this.value - m.value)
    def abs: Time = new Time(java.lang.Math.abs(this.value))

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

  object Time {
    def apply(value: Double): Time = new Time(value)

    def fromDouble(v: Double): Time = {new Time(v)}

    implicit def orderingByValue: Ordering[Time] = {
      Ordering.by(t => t.value)
    }
  }

  object NewTimeNumeric extends Ordering[Time] {
    def compare(x: Time, y: Time): Int = x.value compare y.value
  }

  /** Generation of a UUID. This can be used to generate unique identifiers for objects.
    *
    * @return UUID formatted as a String
    */
  def generateUUID: String = java.util.UUID.randomUUID.toString


  trait Vertex {
    def name: String
    val ID: String
    def center: Position
    def area: Double
    def isInside(pos: Position): Boolean
    def uniformSamplePointInside: Position
    def equalsID(other: Any): Boolean
    override def equals(other: Any): Boolean
    override def hashCode: Int
    override def toString: String
    def nameCompare(n: String): Boolean
    def A: Position
    def B: Position
    def C: Position
    def D: Position

  }

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
  case class VertexRectangle(name: String, A: Position, B: Position, C: Position, D: Position) extends Vertex {

    // unique identifier
    val ID: String = generateUUID

    // center of the rectangle
    val center: Position = A + (B - A)*0.5 + (D - A)*0.5

    // area of the associated zone
    val area: Double = (B - A).norm * (D - A).norm

    /** Is the point inside the vertex ?
      *
      * @param pos [[Position]] to check
      * @return boolean indicating if the point is inside the vertex.
      */
    def isInside(pos: Position): Boolean = {
      val AB: Position = B - A
      val BC: Position = C - B
      val AP: Position = pos - A
      val BP: Position = pos - B
      if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
      else false
    }

    def uniformSamplePointInside: Position = {
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

  class VertexRectangleModifiable(val name: String,
                                  private val _A: (Position, Position),
                                  private val _B: (Position, Position),
                                  private val _C: (Position, Position),
                                  private val _D: (Position, Position)) extends Vertex {

    def getMovableCorners: ((Position, Position),(Position, Position),(Position, Position),(Position, Position)) = {
      (this._A, this._B, this._C, this._D)
    }

    var A: Position = (_A._1 + _A._2) * 0.5
    var B: Position = (_B._1 + _B._2) * 0.5
    var C: Position = (_C._1 + _C._2) * 0.5
    var D: Position = (_D._1 + _D._2) * 0.5
    val center: Position = A + (B - A)*0.5 + (D - A)*0.5

    def updatePositions(fraction: Double): Unit = {
      this.A = this._A._1 + (this._A._2 - this._A._1) * fraction
      this.B = this._B._1 + (this._B._2 - this._B._1) * fraction
      this.C = this._C._1 + (this._C._2 - this._C._1) * fraction
      this.D = this._D._1 + (this._D._2 - this._D._1) * fraction
    }

    val ID: String = generateUUID
    def area: Double = (B - A).norm * (D - A).norm

    def isInside(pos: Position): Boolean = {
      val AB: Position = B - A
      val BC: Position = C - B
      val AP: Position = pos - A
      val BP: Position = pos - B
      if (0 <= (AB dot AP) && (AB dot AP) <= (AB dot AB) && 0 <= (BC dot BP) && (BC dot BP) <= (BC dot BC)) true
      else false
    }

    def uniformSamplePointInside: Position = {
      Vector2D(ThreadLocalRandom.current.nextDouble(A.X+0.1, B.X-0.1), ThreadLocalRandom.current.nextDouble(A.Y+0.1, D.Y-0.1))
    }

    def equalsID(other: Any): Boolean = {
      other match {
        case that: VertexRectangle => this.ID == that.ID
        case _ => false
      }
    }
    override def equals(other: Any): Boolean =
      other match {
        case that: VertexRectangleModifiable => this._A == that._A && this._B == that._B && this._C == that._C && this._D == that._D
        case _ => false
      }

    override def hashCode: Int =  (this._A, this._B, this._C, this._D).##

    override def toString: String = this.name + ", " + this.A + ", " + this.B + ", " + this.C + ", " + this.D
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
  def isInVertex(v: Vertex)(pos: Position): Boolean = {
    val AB: Position = v.B - v.A
    val BC: Position = v.C - v.B
    val AP: Position = pos - v.A
    val BP: Position = pos - v.B
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
