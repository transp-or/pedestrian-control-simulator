package trackingdataanalysis.pedtrack

package object georeferencing {

  def linearTransformation(A:Double, B:Double, C:Double, D:Double, E:Double, F:Double): NewBetterPosition2D => NewBetterPosition2D = {
    (x: NewBetterPosition2D) => new NewBetterPosition2D(A*x.X + B*x.Y + C, D*x.X + E*x.Y + F)
  }

}
