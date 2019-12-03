package hubmodel.mvmtmodels.NOMAD

import hubmodel.ped.PedestrianNOMAD
import myscala.math.vector.Vector2D


class InfluenceAreaReturnPedDataNew(val pedestrianID: Int, val pedestrianSpeed: Vector2D, val pedestrianRadius: Double, val dx: Vector2D, val dxn: Vector2D, val dxt: Vector2D, val dpq: Double, val gpq: Double, val front: Boolean, val vDir: Double)
