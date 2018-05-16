import java.io.{BufferedWriter, File, FileWriter}

import breeze.numerics.{floor, round}
import hubmodel.ped.PedestrianTrait
import myscala.math.vector.{Vector2D, Vector3D}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
/**
  * Created by nicholas on 5/12/17.
  */

package object hubmodel {

  type VehicleID = String
  type VertexID = String
  type StopID = String

  type Position = Vector2D
  type Direction = Vector2D
  type Velocity = Vector2D
  type Acceleration = Vector2D
  type Force = Vector2D

  def distance(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y), 0.5)

  def distance(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X - a.X) * (b.X - a.X) + (b.Y - a.Y) * (b.Y - a.Y) + (b.Z - a.Z) * (b.Z - a.Z), 0.5)


  class Time(val value: Double) extends AnyVal {

    def +(m: Time): Time = new Time(this.value + m.value)

    def addDouble(m: Double): Time = new Time(this.value + m)

    def -(m: Time): Time = new Time(this.value - m.value)

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

    def fromDouble(v: Double): Time = {
      new Time(v)
    }

    implicit def orderingByValue: Ordering[Time] = {
      Ordering.by(t => t.value)
    }

    implicit val readerTimeFromString: Reads[Time] = Reads.of[String].map(s => Time(s.toDouble))

  }

  object NewTimeNumeric extends Ordering[Time] {
    def compare(x: Time, y: Time): Int = x.value compare y.value
  }



  /** Generation of a UUID. This can be used to generate unique identifiers for objects.
    *
    * @return UUID formatted as a String
    */
  def generateUUID: String = java.util.UUID.randomUUID.toString


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
