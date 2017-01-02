package rushhour

/**
  * Created by thomas on 01.01.17.
  */
case class PlacedCar(name: Char, length: Int, head: Coord, isHorizontal: Boolean) {
  def dir: Coord = if(isHorizontal) Coord(1,0) else Coord(0,1)
  def coordinates: Set[Coord] = PlacedCar.generateOccupancy(head, length, isHorizontal)

  def move(dir: Direction): Option[PlacedCar] =
    Some(head + dir.del)
      .filter(newHead => newHead.isValid && (newHead + (dir.del * (length-1))).isValid)
        .map(newHead => this.copy(head = newHead))
}

object PlacedCar {
  def winningCar: PlacedCar = PlacedCar(Board.redCar._1, Board.redCar._2, Coord(4,2), isHorizontal = true)
  def generateOccupancy(head: Coord, length: Int, isHorizontal: Boolean): Set[Coord] =
    if(isHorizontal)
      (for (x <- head.x until (head.x + length)) yield Coord(x, head.y))(collection.breakOut)
    else
      (for (y <- head.y until (head.y + length)) yield Coord(head.x, y))(collection.breakOut)
}
