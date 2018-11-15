package hubmodel.ped

trait WithGraphID {

  var counterGraph: Int = 0

  private var _graph: String = ""

  def graph: String = _graph

  def setGraph(str: String): Unit = {
    if (this.counterGraph == 0) {
      this.counterGraph = 1
      this._graph = str
    } else { throw new UnsupportedOperationException("Graph can only be set once per pedestrian !")}
  }
}
