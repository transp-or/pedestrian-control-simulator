package trackingdataanalysis.pedtrack

import play.api.libs.functional.syntax._
import play.api.libs.json._


/** Processing of the empricial data provided by VisioSafe.
  * The raw data is composed of the position-time coordinates for each pedestrian (millions of lines).
  * This data must be aggregated by pedestrian so an aggregate descriptive analysis can be performed.
  *
  * ==Overview==
  * The work flow is decomposed into two steps. The first is the aggregation into individual pedestrians,
  * which is done by calling [[trackingdataanalysis.pedtrack.visiosafe.SingleDayAggregateProcessor]] or [[trackingdataanalysis.pedtrack.visiosafe.MultiDayAggregateProcessor]].
  * These methods load the data and peform the computations. Once this is done, the second step is the filtering  of
  * the data. This can be done based on time or OD pair, or O/D separately. Finally, the results can be written to
  * text files (CSV or JSON).
  * {{{
  * val pathToData ="path-to-data"
  * val data = new MultiDayProcessor(List(
  *     pathToData + "lausanne_2013_01_22_piw.csv",
  *     pathToData + "lausanne_2013_04_10_piw.csv",
  *     pathToData + "lausanne_2013_04_18_piw.csv",
  *     pathToData + "lausanne_2013_01_23_piw.csv"),
  *   "path-to-coordinate-file.csv")
  *
  * val filterData = data.filterByOD(List(9,10), List(11,12))
  * filterData.writeQuantityToJSON("travelDistance", "travelDistanceExample.json")
  * }}}
  *
  * The same can be done using only one day and not multiple days. One should then use the [[trackingdataanalysis.pedtrack.visiosafe.SingleDayAggregateProcessor]]
  * and the process is similar.
  */
package object visiosafe {

  /** Overrides type from [[trackingdataanalysis.pedtrack]] with emprirical version of pedestrian
    *
    */
  type PedestrianMap = collection.mutable.Map[Int, Pedestrian]

  /** Overrides type from [[trackingdataanalysis.pedtrack]] with empirical verison of pedestrian
    *
    */
  type MultidayPedestrianMap = collection.immutable.Map[DataSpecification, PedestrianMap]

  /**
    * Line through which the flow of pedestrians is calculated.
    * IMPORTANT: the definition of inflow is the following: a person is entering the zone behind the line if he crosses
    * from left to right when standing in point (x1,y1). Therefore when creating these flow lines the order of the
    * points is vry important !
    *
    * @param x1 x-coord of first point
    * @param y1 y-coord of first point
    * @param x2 x-coord of second point
    * @param y2 y-coord of second point
    */
  case class FlowLine_JSON(name: String, x1: Double, y1: Double, x2: Double, y2: Double, controlled: String)

  /**
    * Reads the JSON structure into a [[FlowLine_JSON]] object. No validation on arguments is done.
    */
  implicit val FlowLine_JSONReads: Reads[FlowLine_JSON] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "x1").read[Double] and
      (JsPath \ "y1").read[Double] and
      (JsPath \ "x2").read[Double] and
      (JsPath \ "y2").read[Double] and
      (JsPath \ "").read[String]
    ) (FlowLine_JSON.apply _)


}

