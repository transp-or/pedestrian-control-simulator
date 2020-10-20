package hubmodel.results

import hubmodel.io.input.JSONReaders.{AMWData_JSON, DensityData_JSON, PedestrianResults_JSON}

/** Container for the results from a simulation. This type should be modified if the collectResults function is modified.
  *
  * @param tt                             travel times
  * @param monitoredAreaDensity           density measured inside a single monitored area (optional)
  * @param monitoredAreaIndividualDensity individual density computed inside a single monitored area (optional)
  */
class ResultsContainerReadNew(val id: String,
                              val tt: Vector[PedestrianResults_JSON],
                              val monitoredAreaDensity: Option[Map[String, DensityData_JSON]],
                              val amwData: Option[Vector[AMWData_JSON]]) {

  def addDemandFile(ff: String): ResultsContainerReadWithDemandSetNew = {
    new ResultsContainerReadWithDemandSetNew(this.id, this.tt, this.monitoredAreaDensity, ff)
  }


}

