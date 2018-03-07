/**
  * Created by nicholas on 3/13/17.
  */

package object AggDisagg {


  def poissonProcess(duration: Long, numberPeople: Double, u: breeze.stats.distributions.Uniform, acc: List[Long]): List[Long] = {
    val rate = numberPeople / duration
    if (acc.isEmpty) poissonProcess(duration, numberPeople, u, List(0 - (math.log(u.draw()) / rate).toLong))
    else if (acc.head > duration) acc.tail.reverse
    else poissonProcess(duration, numberPeople, u, acc.head - (math.log(u.draw()) / rate).toLong :: acc)
  }

  //case class Pedestrian(oZone: Int, dZone: Int, entryTime: Double, meanVelocity: Double, travelTime: Double = Double.NaN, exitTime: Double = Double.NaN, travelDistance: Double = Double.NaN)

  /*def flowToArrivals(flow: Flow): Vector[PedestrianInfraOD] = {

    // http://e-collection.library.ethz.ch/eserv/eth:5929/eth-5929-01.pdf
    val meanVelocity: Double = 1.34
    val stdDevVelocity: Double = 0.26

    // https://journals.aps.org/pre/pdf/10.1103/PhysRevE.51.4282


    val zoneId: ZoneId  = ZoneId.systemDefault(); // or: ZoneId.of("Europe/Oslo");

    val arrivalTimes = poissonProcess(flow.end.atZone(zoneId).toEpochSecond - flow.start.atZone(zoneId).toEpochSecond, flow.f, breeze.stats.distributions.Uniform(0,1), List()).map(
      time => flow.start.plusSeconds(time).atZone(zoneId).toEpochSecond
    )

    val velocities = breeze.stats.distributions.Gaussian(meanVelocity, stdDevVelocity).sample(arrivalTimes.length)
    arrivalTimes.zip(velocities).map(t => new PedestrianInfraOD(flow.O, flow.D, 1000*math.max(0.5, math.min(1.3*1.34, t._2)), t._1)).toVector
  }

  def disaggregateInflow(inflow: Vector[Flow]): Vector[PedestrianInfraOD] = {
    inflow.flatMap(i => flowToArrivals(i))
  }*/
}

