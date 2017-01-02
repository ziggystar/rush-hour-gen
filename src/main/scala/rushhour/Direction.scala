package rushhour

/**
  * Created by thomas on 01.01.17.
  */
sealed trait Direction {
  def del: Coord
}

case object Up extends Direction {
  val del: Coord = Coord(0,-1)
}
case object Down extends Direction {
  val del: Coord = Coord(0,1)
}
case object Left extends Direction {
  val del: Coord = Coord(-1,0)
}
case object Right extends Direction {
  val del: Coord = Coord(1,0)
}
