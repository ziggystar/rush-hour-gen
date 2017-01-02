package rushhour

/**
  * Created by thomas on 01.01.17.
  */
case class Coord(x: Int, y: Int) {
  def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  def *(scalar: Int): Coord  = Coord(x * scalar, y * scalar)
  def isValid: Boolean = math.min(x,y) >= 0 && math.max(x,y) < 6
}

object Coord {
  def allCoordinates: Set[Coord] = (for(x <- 0 to 5; y <- 0 to 5) yield Coord(x,y))(collection.breakOut)
}