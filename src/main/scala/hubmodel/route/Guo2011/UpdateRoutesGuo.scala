package hubmodel.route.Guo2011

import hubmodel.DES.{Action, PedestrianDES}
import hubmodel.ped.{PedestrianNOMAD, PedestrianSim}

import scala.reflect.ClassTag


class UpdateRoutesGuo[T <: PedestrianNOMAD](sim: PedestrianDES[T]) extends Action {


  override def execute(): Unit = ???

}
