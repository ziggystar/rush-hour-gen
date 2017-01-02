package rushhour

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
}