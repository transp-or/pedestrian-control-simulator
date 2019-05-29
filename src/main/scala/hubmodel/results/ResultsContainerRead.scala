package hubmodel.results

import hubmodel.io.input.JSONReaders.PedestrianResults_JSON

/** Container for the results from a simulation. This type should be modified if the collectResults function is modified.
  *
  * @param tt                             travel times
  * @param monitoredAreaDensity           density measured inside a single monitored area (optional)
  * @param monitoredAreaIndividualDensity individual density computed inside a single monitored area (optional)
  */
@deprecated
case class ResultsContainerRead(tt: Vector[(String, String, Double, Double, Double, Double)],
                                monitoredAreaDensity: Option[(Vector[Double], Vector[Vector[Double]])],
                                monitoredAreaIndividualDensity: Option[Vector[(BigDecimal, BigDecimal)]]) {


  def addDemandFile(ff: String): ResultsContainerReadWithDemandSet = {
    new ResultsContainerReadWithDemandSet(this.tt, this.monitoredAreaDensity, this.monitoredAreaIndividualDensity, ff)
  }
}

/** Container for the results from a simulation. This type should be modified if the collectResults function is modified.
  *
  * @param tt                             travel times
  * @param monitoredAreaDensity           density measured inside a single monitored area (optional)
  * @param monitoredAreaIndividualDensity individual density computed inside a single monitored area (optional)
  */
case class ResultsContainerReadNew(tt: Vector[PedestrianResults_JSON],
                                   monitoredAreaDensity: Option[(Vector[Double], Vector[Vector[Double]])],
                                   monitoredAreaIndividualDensity: Option[Vector[(BigDecimal, BigDecimal)]]) {


  def addDemandFile(ff: String): ResultsContainerReadWithDemandSetNew = {
    new ResultsContainerReadWithDemandSetNew(this.tt, this.monitoredAreaDensity, this.monitoredAreaIndividualDensity, ff)
  }
}
