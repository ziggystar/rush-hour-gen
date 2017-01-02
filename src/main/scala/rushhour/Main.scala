package rushhour

import scala.util.Random
import Board.syntax._

/**
  * Created by thomas on 01.01.17.
  */
object Main {
  def main(args: Array[String]): Unit = {
    (1 to 50).foreach{ s =>
      println(s)
      probe(new Random(s))
    }
  }
  def probe(random: Random): Unit = {
    val p = 0.9
    val cars = Board.allCars.filter(_._1 != 'a').filter{case (n,l) => random.nextDouble() < p}
    //println(cars)
    val board: Option[CarBoard] = generateWinning(random, cars.toSeq)

    //println(board.map(_.show).getOrElse("not found"))

    val implBoard = implicitly[Board[CarBoard]]
    val graph = implBoard.unfoldGraph(board.get)
    println(s"graph has ${graph.nodes.size} nodes")
    //println(graph.nodes.map(_.show).mkString("\n\n"))
  }

  def generateWinning(rand: Random, toPlace: Seq[Board.CarPrototype]): Option[CarBoard] = {
    val initial = CarBoard(Set(PlacedCar.winningCar))
    toPlace.foldLeft(Some(initial): Option[CarBoard]){
      case (None,_)            => None
      case (Some(board),proto) => addCar(board,proto,rand)
    }
  }

  def addCar(b: CarBoard, car: (Char,Int), rand: Random): Option[CarBoard] = {
    val candidates: Seq[PlacedCar] = for {
      headX <- 0 to 5
      headY <- 0 to 5
      head = Coord(headX,headY)
      isHorizontal <-
        Seq(true,false)
        if PlacedCar.generateOccupancy(head, car._2, isHorizontal).forall(c => !b.isOccupied(c) && c.isValid)
    } yield PlacedCar(car._1, car._2, head, isHorizontal)

    rand.shuffle(candidates).headOption.map(pc => b.copy(cars = b.cars + pc))
  }
}
