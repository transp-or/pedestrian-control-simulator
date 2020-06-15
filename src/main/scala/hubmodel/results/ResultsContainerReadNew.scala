package hubmodel.results

import hubmodel.io.input.JSONReaders.{AMWData_JSON, PedestrianResults_JSON}

/** Container for the results from a simulation. This type should be modified if the collectResults function is modified.
  *
  * @param tt                             travel times
  * @param monitoredAreaDensity           density measured inside a single monitored area (optional)
  * @param monitoredAreaIndividualDensity individual density computed inside a single monitored area (optional)
  */
class ResultsContainerReadNew(val tt: Vector[PedestrianResults_JSON],
                              val monitoredAreaDensity: Option[Map[String, Vector[(tools.Time, Double)]]],
                              val monitoredAreaIndividualDensity: Option[Map[(String, String), Vector[(tools.Time, Vector[Double])]]],
                              val amwData: Option[Vector[AMWData_JSON]]) {

  def addDemandFile(ff: String): ResultsContainerReadWithDemandSetNew = {
    new ResultsContainerReadWithDemandSetNew(this.tt, this.monitoredAreaDensity, this.monitoredAreaIndividualDensity, ff)
  }

}

