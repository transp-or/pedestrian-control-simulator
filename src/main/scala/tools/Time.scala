package tools

import java.math.MathContext

import play.api.libs.json.Reads

import scala.math.BigDecimal.RoundingMode

class Time(val value: BigDecimal) extends AnyVal {

  def +(m: Time): Time = new Time(this.value + m.value)

  def addDouble(m: Double): Time = new Time(this.value + m)

  def -(m: Time): Time = new Time(this.value - m.value)

  def abs: Time = new Time(this.value.abs)

  def *(m: Double) = new Time(this.value * m)

  def asReadable: String = {
    val hours: Int = (this.value / 3600.0).setScale(0, RoundingMode.FLOOR).toInt
    val minutes: Int = ((this.value - hours * 3600) / 60.0).setScale(0, RoundingMode.FLOOR).toInt
    val seconds: Int = (this.value - 3600 * hours - 60 * minutes).setScale(0, RoundingMode.FLOOR).toInt
    f"${hours}%02d" + ":" + f"${minutes}%02d" + ":" + f"${seconds}%02d"
  }

  def asVisioSafe: String = {
    val h: Int = (this.value / 3600.0).setScale(0, RoundingMode.FLOOR).toInt
    val min: Int = ((this.value - h * 3600) / 60.0).setScale(0, RoundingMode.FLOOR).toInt
    val s: Int = (this.value - 3600 * h - 60 * min).setScale(0, RoundingMode.FLOOR).toInt
    val ms: Int = (1000 * (this.value - 3600 * h - 60 * min - s)).round(new MathContext(4, java.math.RoundingMode.HALF_UP)).toInt
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

  implicit val readerTimeFromString: Reads[Time] = Reads.of[String].map(s => {
    if (s.contains(":")) {
      val hms = s.split(":").map(_.toInt)
      Time(hms(0)*3600 + hms(1)*60 + hms(2))
    } else {
      Time(s.toDouble)
    }
  })

}

object TimeNumeric extends Ordering[Time] {
  def compare(x: Time, y: Time): Int = x.value compare y.value
}