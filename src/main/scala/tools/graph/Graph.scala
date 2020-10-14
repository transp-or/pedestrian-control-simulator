package tools.graph

import hubmodel.supply.graph.{MyRawEdgeDEMANDESTIMATION}
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.DefaultDirectedWeightedGraph
import tools.cells.{Rectangle, Vertex}

import scala.jdk.CollectionConverters.{ListHasAsScala}

import scala.util.{Failure, Success, Try}

class Graph[T <: Vertex](vertices: Iterable[T], edges: Iterable[MyRawEdgeDEMANDESTIMATION[T]]) {

  private val vertexCollection: Map[String, T] = vertices.map(v => v.name -> v).toMap

  private val edgeCollection: Map[(T, T), MyRawEdgeDEMANDESTIMATION[T]] = edges.map(e => (e.startVertex, e.endVertex) -> e).toMap

  private val network: DefaultDirectedWeightedGraph[T, MyRawEdgeDEMANDESTIMATION[T]] = new DefaultDirectedWeightedGraph[T, MyRawEdgeDEMANDESTIMATION[T]](classOf[MyRawEdgeDEMANDESTIMATION[T]])

  this.vertexCollection.values.foreach(network.addVertex)

  // adds the edges and sets the weights
  this.edgeCollection.values.foreach(e => {
    network.addEdge(e.startVertex, e.endVertex, e)
    network.setEdgeWeight(e, e.cost)
  })


  // object used to get the shortest path in the network
  private var shortestPathBuilder: DijkstraShortestPath[T, MyRawEdgeDEMANDESTIMATION[T]] = new DijkstraShortestPath[T, MyRawEdgeDEMANDESTIMATION[T]](network)


  /** Uses to shortestPathBuilder to compute the shortest path between two vertices.
    *
    * @param o origin node
    * @param d destination node
    * @return the list if vertices representing the path
    */
  def getShortestPath(o: T, d: T): (Double, List[Vertex]) = {

    Try(shortestPathBuilder.getPath(o, d)) match {
      case Success(s) if s.getVertexList.size() > 0 => {
        (s.getWeight, s.getVertexList.asScala.toList)
      }
      case Failure(f) => {
        throw f
      }
      case _ => {
        throw new IllegalAccessError("No route from " + o + " to " + d)
      }
    }
  }


  def getShortestPaths(o: T): Map[(T, T), (Double, List[T])] = {

    Try(shortestPathBuilder.getPaths(o)) match {
      case Success(s) => {
        this.vertices.map(v => {
          (o, v) -> (s.getPath(v).getWeight, s.getPath(v).getVertexList.asScala.toList)
        }).toMap
      }
      case Failure(f) => {
        throw f
      }
    }
  }


}
