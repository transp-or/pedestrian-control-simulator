package hubmodel.io.input.JSONReaders

import hubmodel.supply.Infrastructure

/**
  * Class which reads the full graph file. All elements are read from the file and the config file specifies which
  * elements are used. The combination of multiple control strategies is possible.
  *
  * @param amwsMode            geographical location
  * @param subLocation         sublocation within the hub
  * @param nodes               list of nodes
  * @param standardConnections connectivity
  * @param flowGates           collection of flow gates
  * @param controlledAreas     collection of controlled areas
  * @param binaryGates         collection of binary gates
  * @param movingWalkways      collection of moving walkways
  * @param flowSeparators      collection of flow separators
  */
case class InfraGraphParser(amwsMode: String,
                            subLocation: String,
                            nodes: Vector[Vertex_JSON],
                            standardConnections: Vector[Connectivity_JSON],
                            levelChanges: Vector[Connectivity_JSON],
                            flowGates: Vector[FlowGates_JSON],
                            controlledAreas: Vector[MonitoredAreas_JSON], // the controlled areas are the same as the zones
                            binaryGates: Vector[FlowGates_JSON], // binary gates are flow gates with only two states
                            movingWalkways: Vector[MovingWalkways_JSON],
                            flowSeparators: Vector[FlowSeparator_JSON],
                            alternateConnections: Vector[ConnectivityAlternatives_JSON],
                            destinationEquivalencies: Vector[destinationGroup_JSON]
                           ) extends Infrastructure
