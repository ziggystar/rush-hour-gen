package rushhour

import scala.util.Random

/**
  * Created by thomas on 01.01.17.
  */
case class CarBoard(cars: Set[PlacedCar]) {
  override def toString: String = CarBoard.implBoard.show(this)
}

object CarBoard {
  case class Move(from: PlacedCar, to: PlacedCar, dir: Direction) {
    def apply(board: CarBoard): CarBoard = {
      require(
        board.cars.contains(from),
        "board does not contain moved car")
      require(
        (board.cars - from).forall(c => c.coordinates.intersect(to.coordinates).isEmpty),
        "move moves into other car")
      CarBoard((board.cars - from) + to)
    }
  }

  implicit val implBoard: Board[CarBoard] = new Board[CarBoard] {
    type Move = CarBoard.Move

    override def cars(b: CarBoard): Set[PlacedCar] = b.cars

    override def move(b: CarBoard, m: Move): CarBoard = m(b)

    override def moves(b: CarBoard): Set[Move] = {
      val occupied: Set[Coord] = b.cars.flatMap(_.coordinates)
      for {
        c <- b.cars
        move <- if (c.isHorizontal) Set(Left, Right) else Set(Up, Down)
        movedCar <- c.move(move) if movedCar.coordinates.intersect(occupied -- c.coordinates).isEmpty
      } yield Move(c, movedCar, move)
    }
  }
  def generateBoard(p: Double, random: Random): Option[CarBoard] = {
    val cars = Board.allCars.filter(_._1 != 'a').filter{case (n,l) => random.nextDouble() < p}
    //println(cars)
    generateWinning(random, cars.toSeq)
  }

  def generateWinning(rand: Random, toPlace: Seq[Board.CarPrototype]): Option[CarBoard] = {
    val initial = CarBoard(Set(PlacedCar.winningCar))
    toPlace.foldLeft(Some(initial): Option[CarBoard]){
      case (None,_)            => None
      case (Some(board),proto) => addCar(board,proto,rand)
    }
  }

  def addCar(b: CarBoard, car: (Char,Int), rand: Random): Option[CarBoard] = {
    import Board.syntax._

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