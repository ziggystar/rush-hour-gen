package rushhour.impl

import rushhour.Board.CarPrototype
import rushhour.{Board, CarBoard, Coord, PlacedCar}

/**
  * Created by thomas on 20.01.17.
  */
case class HeadBoard(vertical: IndexedSeq[Seq[CarPrototype]], horizontal: IndexedSeq[Seq[CarPrototype]]) { outer =>
  require(
    (vertical.flatten ++ horizontal.flatten).distinct.size == (vertical ++ horizontal).map(_.size).sum,
    "some cars are placed multiple times")
  require(vertical.forall(_.map(_._2).sum <= 6), "a column is more than full")
  require(horizontal.forall(_.map(_._2).sum <= 6), "a row is more than full")
  require(horizontal(2).contains(Board.redCar), "red car is not placed on third row")

  val vertByCar: IndexedSeq[(CarPrototype,Int)] = vertical.zipWithIndex.flatMap{case (cps,idx) => cps.map(_ -> idx)}
  val horzByCar: IndexedSeq[(CarPrototype,Int)] = horizontal.zipWithIndex.flatMap{case (cps,idx) => cps.map(_ -> idx)}

  /** index of winning car in horzByCar. */
  val winningIndex: Int = horzByCar.indexOf(Board.redCar -> 2)

  implicit val board: Board[State] = new Board[State] {
    override type Move = outer.Move
    override def cars(b: State): Set[PlacedCar] = b.placedCars
    override def move(b: State, m: Move): State = b.move(m)
    override def moves(b: State): Set[Move] = allMoves.filter(b.isValidMove).toSet

    override def isWinning(b: State): Boolean = b.offsetHorizontals(winningIndex) == 4
    override def isOccupied(b: State)(coord: Coord): Boolean = b.isOccupied(coord)
  }

  case class Move(isVertical: Boolean, carIdx: Int, offset: Int)
  def allMoves: IndexedSeq[Move] =
    vertByCar.indices.flatMap(idx => Seq(Move(isVertical = true, idx, -1), Move(isVertical = true,idx,+1)) ++
      horzByCar.indices.flatMap(idx => Seq(Move(isVertical = false, idx, -1), Move(isVertical = false,idx,+1))))

  /** A `State` states the missing coordinate (e.g. row for vertical cars) of the head of each car. */
  case class State(offsetVerticals: Array[Int], offsetHorizontals: Array[Int]){
    /** Each byte is a row. */
    val board: BitBoard = {
      var b = BitBoard.empty
      var i = 0
      while(i < offsetVerticals.length) {
        b = b.addVerticalCar(x = vertByCar(i)._2, y = offsetVerticals(i), length = vertByCar(i)._1._2)
        i = i + 1
      }
      i = 0
      while(i < offsetHorizontals.length) {
        b = b.addHorizontalCar(x = horzByCar(i)._2, y = offsetHorizontals(i), length = horzByCar(i)._1._2)
        i = i + 1
      }
      b
    }

    def placedCars: Set[PlacedCar] =
      offsetVerticals.zip(vertByCar)
        .map{case (off, (cp,col)) => PlacedCar(cp._1,cp._2,Coord(off,col), isHorizontal = false)}
        .++(
          offsetHorizontals.zip(horzByCar)
            .map{case (off, (cp,row)) => PlacedCar(cp._1,cp._2,Coord(off,row), isHorizontal = true)}
        )(collection.breakOut)

    def isOccupied(c: Coord): Boolean = board.get(c.x,c.y)

    def move(m: Move): State =
      if(m.isVertical) this.copy(offsetVerticals = offsetVerticals.updated(m.carIdx, offsetVerticals(m.carIdx) + m.offset))
      else this.copy(offsetHorizontals = offsetHorizontals.updated(m.carIdx, offsetHorizontals(m.carIdx) + m.offset))

    def isValidMove(m: Move): Boolean = {
      val currentOffsets = if(m.isVertical) offsetVerticals else offsetHorizontals
      val oldHead = currentOffsets(m.carIdx)
      val newHead = m.offset + oldHead
      val ((_,length),fixedCoord)  = (if(m.isVertical) vertByCar else horzByCar)(m.carIdx)
      val newEnd = newHead + length - 1
      //do the cheap test for being within board boundaries first
      newHead >= 0 && newEnd < 6 && {
        //compute the range of the non-fixed coordinate that has to be checked
        val checkRange = if(newHead < oldHead) newHead until oldHead else (oldHead + length) until newEnd
        if(m.isVertical) checkRange.forall(y => !board.get(fixedCoord,y))
        else checkRange.forall(x => !board.get(x,fixedCoord))
      }
    }
    def getBoard: HeadBoard.this.type = outer
  }

  def stateFromCarBoard(cb: CarBoard): Option[State] = {
    val (h,v) = cb.cars.partition(_.isHorizontal)
    val hs: Array[Int] = horzByCar.flatMap{
          case ((name,length), y) => h.find(c => c.name == name && c.length == length && c.head.y == y).map(_.head.x)
        }(collection.breakOut)
    val vs: Array[Int] = vertByCar.flatMap{
      case ((name,length), x) => h.find(c => c.name == name && c.length == length && c.head.x == x).map(_.head.y)
    }(collection.breakOut)
    if(hs.length == horzByCar.length && vs.length == vertByCar.length) Some(State(vs, hs)) else None
  }
}

object HeadBoard {
  def fromCarBoard(cb: CarBoard): HeadBoard#State = {
    val (hors, verts) = cb.cars.partition(_.isHorizontal)
    val vs: IndexedSeq[Seq[(Char, Int)]] = verts
      .groupBy(_.head.x)
      .toIndexedSeq
      .sortBy(_._1)
      .map(_._2.toSeq.sortBy(_.head.y).map(pc => (pc.name, pc.length)))
    val hs: IndexedSeq[Seq[(Char, Int)]] = hors
      .groupBy(_.head.y)
      .toIndexedSeq
      .sortBy(_._1)
      .map(_._2.toSeq.sortBy(_.head.x).map(pc => (pc.name, pc.length)))
    val hb = HeadBoard(vs, hs)
    hb.stateFromCarBoard(cb).get
  }
}

case class BitBoard(b: Long) extends AnyVal {
  def get(x: Int, y: Int): Boolean = (BitBoard.bitAt(x,y) & b) != 0
  def set(x: Int, y: Int): BitBoard = BitBoard(b | BitBoard.bitAt(x,y))
  def addVerticalCar(x: Int, y: Int, length: Int): BitBoard = length match {
    case 2 => this.set(x,y).set(x,y+1)
    case 3 => this.set(x,y).set(x,y+1).set(x,y+2)
  }
  def addHorizontalCar(x: Int, y: Int, length: Int): BitBoard = length match {
    case 2 => this.set(x,y).set(x+1,y)
    case 3 => this.set(x,y).set(x+1,y).set(x+2,y)
  }
}

object BitBoard {
  val empty: BitBoard = BitBoard(0L)
  val byteHotAt: Array[Int] = Array.iterate(1,7)(_ << 1)
  @inline
  def bitAt(x: Int, y: Int): Long = byteHotAt(x).toLong >> (8*y)
}
