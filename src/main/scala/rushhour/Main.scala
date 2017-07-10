package rushhour

import scala.util.Random
import Board.syntax._
import rushhour.graph.Graph

/**
  * Created by thomas on 01.01.17.
  */
object Main {

  val implBoard: Board[CarBoard] = implicitly[Board[CarBoard]]
  type Move = implBoard.Move

  case class SolvedGame(graph: Graph[CarBoard,Move]){
    val usedCars: Set[(Char, Int)] = graph.nodes.head.cars.map(pc => (pc.name, pc.length))
    val solutionNodes: Set[CarBoard] = graph.nodes.filter(_.isWinning)
    val cost: Map[CarBoard,Int] = {
      def buildCosts(acc: Map[CarBoard,Int] = Map(),
                     fringe: Set[CarBoard] = solutionNodes,
                     fringeCost: Int = 0): Map[CarBoard,Int] =
        if(fringe.isEmpty) acc
        else {
          buildCosts(
            acc ++ fringe.map(_ -> fringeCost),
            fringe.flatMap(b => graph.neighbours(b).map(_._2)).filterNot(acc.keySet).filterNot(fringe),
            fringeCost + 1)
        }
      buildCosts()
    }
    val longestPath: Int = cost.values.max
    val hardestNode: CarBoard = cost.maxBy(_._2)._1
  }

  def solveGame(b: CarBoard): SolvedGame = SolvedGame(implBoard.unfoldGraph(b))

  def main(args: Array[String]): Unit = {
    val random = new Random(0)
    Stream
      .continually(CarBoard.generateBoard(0.7,random))
      .flatten
      .map(solveGame)
      .filter(_.longestPath >= 15)
      .take(5)
      .foreach { s =>
      println(s"path: ${s.longestPath}")
      println(s.hardestNode)
    }
  }
}
