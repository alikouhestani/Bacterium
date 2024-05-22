



object Bacterium extends App {
  case class Position(x: Int, y: Int) {
    lazy val neighbors: List[Position] = {
      (for {
        neighborY <- this.y -1 to this.y + 1
        neighborX <- this.x -1 to this.x + 1
        pos=   Position(neighborX, neighborY)
        if pos != this && pos.x >= 0 && pos.y >= 0
      } yield pos).toList
    }
  }

  object Board {
    def apply(aliveBacteriumCoordinates : Set[Position]): Board = {

      val elements = for {
        x <- 0 until aliveBacteriumCoordinates.maxBy(_.x).x +10
        y <- 0 until aliveBacteriumCoordinates.maxBy(_.y).y +10
        position= Position(x,y)
      } yield {
        if (aliveBacteriumCoordinates.contains(position)) position-> Alive else position -> Dead
      }
      new Board(elements.toMap)
    }
  }
  case class Board(bacterium: Map[Position, State]) {


    private def neighborsAlive(position: Position): Int ={
      position.neighbors.count(neighbor => bacterium.getOrElse(neighbor, Dead) == Alive)
    }


    private val survives = { (pos: Position, board: Map[Position, State]) =>
      val aliveNeighbors = neighborsAlive(pos)
      (board(pos) == Alive && aliveNeighbors == 2) || aliveNeighbors == 3
    }

    private val next = { current: Board =>
      val newBacterium = current.bacterium.map { case (pos: Position, _: State) =>
        if (survives(pos, current.bacterium)) pos -> Alive
        else pos -> Dead
      }
      current.copy(bacterium = newBacterium)
    }

    def generation(iterations: Int = 1): Unit = {
      LazyList.iterate(this)(next).take(iterations).zipWithIndex.foreach { case (board, i) =>
        println(s"In generation $i, bacterium with these coordinates survived : ${board.bacterium.filter(_._2 == Alive).keys}")
      }

    }
  }

  val smallBoard = Board(Set(Position(1,2), Position(2,2), Position(3,2)))

  val bigBoard = Board(Set(Position(1001,1002), Position(1002,1002), Position(1003,1002)))
  smallBoard.generation(3)
  bigBoard.generation(3)
}



