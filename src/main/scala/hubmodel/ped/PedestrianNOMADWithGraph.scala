package hubmodel.ped

import hubmodel.tools.cells.Rectangle
import hubmodel.{Position, Time}

class PedestrianNOMADWithGraph(o: Rectangle,
                               d: Rectangle,
                               et: Time,
                               pos: Position) extends PedestrianNOMAD(o, d, et, pos) with WithGraphID
