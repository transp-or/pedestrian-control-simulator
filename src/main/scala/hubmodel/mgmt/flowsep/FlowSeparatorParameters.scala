package hubmodel.mgmt.flowsep

import hubmodel.Position
import hubmodel.mgmt.{FunctionalForm, Measurement, Output}

/** Container for all the parameters need to build a [[FlowSeparator]].
  *
  * @param startA                 extreme position of start
  * @param startB                 other extreme position of start
  * @param endA                   extreme position of end
  * @param endB                   other extrem position of end
  * @param inflowLinesStart       lines across which pedestrian flow is mesured for the start
  * @param inflowLinesEnd         lines across which pedestrian flow is mesured for the end
  * @param associatedZonesStart   zones to add in the graph for this separator
  * @param associatedZonesEnd     zones to add in the graph for this separator
  * @param associatedConnectivity connectivity of the new zones
  * @param overriddenZones        zones to delete
  * @param function               function linking the KPI to the position of the separator.
  * @tparam T measurement type
  * @tparam U output type
  */
case class FlowSeparatorParameters[T <: Measurement, U <: Output](startA: Position,
                                                                  startB: Position,
                                                                  endA: Position,
                                                                  endB: Position,
                                                                  inflowLinesStart: Iterable[FlowLineParameters],
                                                                  inflowLinesEnd: Iterable[FlowLineParameters],
                                                                  associatedZonesStart: Iterable[RectangleModifiableParameters],
                                                                  associatedZonesEnd: Iterable[RectangleModifiableParameters],
                                                                  associatedConnectivity: Iterable[(String, String)],
                                                                  function: FunctionalForm[T, U]) {

  def changeFunction(f: FunctionalForm[T, U]): FlowSeparatorParameters[T, U] = copy(function = f)

}
