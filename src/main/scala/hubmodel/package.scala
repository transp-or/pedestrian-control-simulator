import breeze.numerics.{floor, round}
import myscala.math.linalg.areaFrom2DVectors
import breeze.linalg.DenseVector

/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {

  // Type representing a 2D position
  type Position = breeze.linalg.DenseVector[Double]

  // Type representing a 2D direction
  type Direction = breeze.linalg.DenseVector[Double]

  // Type representing a 2D velocity
  type Velocity = breeze.linalg.DenseVector[Double]

  // Type representing a 2D acceleration
  type Acceleration = breeze.linalg.DenseVector[Double]

  // Type representing a 2D force
  type Force = breeze.linalg.DenseVector[Double]

  // Type for representing Time: in seconds !
  type Time = Double

  /** Implicit conversion for printing the [[hubmodel.Time]] type. Covnerts the Double to readable
    * time for humans. Converts the seconds to hours, minutes and seconds of the day.
    *
    * @param t Time to convert
    */
  implicit class timePrint(t: Time){

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
      val hours: Int = floor(t/3600.0).toInt
      val minutes: Int = floor((t - hours*3600)/60.0).toInt
      val seconds: Double = t - hours*3600 - minutes*60
      hours.toString + ":" + minutes.toString + ":" + seconds.toString
    }
  }

  /** Takes a [[hubmodel.Time]] as argument and converts it to a readable time.
    *
    * @param t [[hubmodel.Time]] to convert
    * @return formatted as hh::mm::ss
    */
  def timeReadable(t: Time): String = {
    val hours: Int = floor(t/3600.0).toInt
    val minutes: Int = floor((t - hours*3600)/60.0).toInt
    val seconds: Double = t - hours*3600 - minutes*60
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
    * @param A bottom left
    * @param B bottom right
    * @param C top right
    * @param D top left
    */
  case class Vertex(name: String, A: Position, B: Position, C: Position, D: Position) {

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
  }

  /** Function to check whether a [[hubmodel.Position]] is inside a [[hubmodel.Vertex]]. The default Vertex is a
    * plain rectangle, and hence this function checks whether the point is inside the rectangle. For more sophisticated
    * shapes, this function must be overriden.
    *
    * @param v vertex
    * @param pos position ot check
    * @return boolean indicating if the point is inside the vertex
    */
  def isInVertex(v: Vertex)(pos: Position): Boolean = {
    val AB: DenseVector[Double] = v.B - v.A
    val BC: DenseVector[Double] = v.C - v.B
    val AP: DenseVector[Double] = pos - v.A
    val BP: DenseVector[Double] = pos - v.B
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
