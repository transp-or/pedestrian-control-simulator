package hubmodel.results

/** Container for the results from a simulation. This type should be modified if the collectResults function is modified.
  *
  * @param tt travel times
  * @param monitoredAreaDensity density measured inside a single monitored area (optional)
  * @param monitoredAreaIndividualDensity individual density computed inside a single monitored area (optional)
  */
case class ResultsContainerRead(tt: Vector[(String, String, Double, Double, Double, Double)],
                                monitoredAreaDensity: Option[(Vector[Double], Vector[Vector[Double]])],
                                monitoredAreaIndividualDensity: Option[Vector[(BigDecimal, BigDecimal)]]) {


  def addDemandFile(ff: String): ResultsContainerReadWithDemandSet = {
    new ResultsContainerReadWithDemandSet(this.tt, this.monitoredAreaDensity, this.monitoredAreaIndividualDensity, ff)
  }
}
