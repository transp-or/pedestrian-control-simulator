package hubmodel.ped

import hubmodel.{Position, Time}
import hubmodel.tools.cells.Rectangle

class PedestrianNOMADWithGraph(o: Rectangle,
                               d: Rectangle,
                               et: Time,
                               pos: Position,
                               route: List[Rectangle],
                               val graph: String = "") extends PedestrianNOMAD(o, d, et, pos, route) with GraphID
