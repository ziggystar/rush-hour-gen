package rushhour.graph

/** An undirected graph.
  */
case class Graph[V,E](nodes: Set[V], neighbours: Map[V,Set[(E,V)]])

object Graph {
  def unfold[V,E](v: V)(succ: V => Iterable[(E,V)]): Graph[V,E] = {
    Stream.iterate((Graph[V,E](Set(v),Map()), Set[V]())){
      case (graph, closed) if graph.nodes.size == closed.size => (graph, closed)
      case (Graph(nodes,mapping), closed) =>
        val next: V = (nodes -- closed).head
        val succs: Iterable[(E, V)] = succ(next)
        val newGraph: Graph[V, E] = Graph(nodes ++ succs.map(_._2), mapping + (next -> succs.toSet))
        (newGraph, closed + next)
    }.dropWhile{ case (g,c) => g.nodes != c }.head._1
  }
}


