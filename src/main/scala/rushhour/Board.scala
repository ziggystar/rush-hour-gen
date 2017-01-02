package rushhour

import rushhour.graph.Graph


/** Type-class for representing a rush hour board, a state of the game.
  *
  * A Board can be in the winning position.
  * And a Board has a set of reachable neighbouring positions.
  * The type class represents a high-level interface for getting the contents of a board (the cars).
  */
trait Board[B] {
  type Move
  def cars(b: B): Set[PlacedCar]
  def moves(b: B): Set[Move]
  def move(b: B, m: Move): B
  def getCar(b: B)(carName: Char): Option[PlacedCar] = cars(b).find(_.name == carName)
  def isWinning(b: B): Boolean = getCar(b)(Board.redCar._1).exists(_.coordinates == Board.winningPosition)
  def isOccupied(b: B)(coord: Coord): Boolean = cars(b).flatMap(_.coordinates).contains(coord)
  def show(b: B): String = {
    val charAt: Map[Coord,Char] = (for {
      car <- cars(b)
      coord <- car.coordinates
    } yield (coord, car.name)).toMap.withDefaultValue('.')
    (for {
      line <- 0 to 5
    } yield (for (x <- 0 to 5) yield charAt(Coord(x, line))).mkString).mkString("\n")
  }
  def unfoldGraph(b: B): Graph[B,Move] = Graph.unfold(b){ bb =>
    moves(bb).map(m => (m,move(bb,m)))(collection.breakOut)
  }
}

object Board{
  type CarPrototype = (Char,Int)
  def allCars: Set[CarPrototype] = Set('a','b','c','d','e','f','g','h').map(_ -> 2) ++ Set('U','V','W','X').map(_ -> 3)
  def redCar: CarPrototype = ('a',2)
  def winningPosition: Set[Coord] = Set(Coord(5,2),Coord(4,2))

  object syntax {
    implicit class RichBoard[B](b: B)(implicit impl: Board[B]){
      def cars: Set[PlacedCar] = impl.cars(b)
      def getCar: (Char) => Option[PlacedCar] = impl.getCar(b)
      def isWinning: Boolean = impl.isWinning(b)
      def isOccupied: (Coord) => Boolean = impl.isOccupied(b)
      def show: String = impl.show(b)
    }
  }
}
