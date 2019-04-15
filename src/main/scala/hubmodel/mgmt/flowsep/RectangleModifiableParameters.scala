package hubmodel.mgmt.flowsep

import hubmodel.Position

case class RectangleModifiableParameters(name: String,
                                         A: (Position, Position),
                                         B: (Position, Position),
                                         C: (Position, Position),
                                         D: (Position, Position),
                                         od: Boolean,
                                         genRate: Option[Double])
