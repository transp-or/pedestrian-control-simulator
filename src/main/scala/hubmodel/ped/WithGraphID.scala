package hubmodel.ped

trait WithGraphID {

  private var counter_graph: Int = 0

  var _graph: String = ""

  def graph: String = _graph

  def setGraph(str: String): Unit = {
    if (this.counter_graph == 0) { this._graph = str  }
    else { throw new UnsupportedOperationException("Graph can only be set once per pedestrian !")}
  }
}
