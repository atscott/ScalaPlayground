package CodinGame.TheGreatEscape

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player {

  case class Position(x: Int = -1, y: Int = -1)

  case class PlayerClass(var position: Position, var wallsLeft: Int, id: Int)

  case class Board(players: List[PlayerClass], width: Int, height: Int, var walls: List[Wall]) {

  }

  case class Wall(wallX: Int, wallY: Int, orientation: Char, size: Int = 2)

  class PathDeterminer(board: Board) {

    def getPathFromPredecessorMap(start: Position, end: Position, predecessors: Map[Position, Position]): List[Position] = {
      if (!predecessors.get(end).isDefined || end == start) List()
      else getPathFromPredecessorMap(start, predecessors.get(end).get, predecessors) ++ List(end)
    }

    def shortestPathToFinish(playerId: Int): List[Position] = {
      val targets = if (playerId == 0) for (y <- 0 until board.height) yield Position(board.width - 1, y)
      else if (playerId == 1) for (y <- 0 until board.height) yield Position(0, y)
      else if (playerId == 2) for (x <- 0 until board.width) yield Position(x, board.height - 1)
      else for (x <- 0 until board.width) yield Position(x, 0)

      val start = board.players.filter(f => f.id == playerId).head.position
      val predecessors = BFS(targets.toSet, Set(start), Set(start), Map())
      getPathFromPredecessorMap(start, predecessors.find(p => targets.contains(p._1)).get._1, predecessors)
    }

    def BFS(goals: Set[Position], queue: Set[Position], visited: Set[Position], predecessors: Map[Position, Position]): Map[Position, Position] = {
      if (queue.isEmpty) return predecessors

      val unvisitedNeighbors = (for {
        pos <- queue
        neighbor <- neighbors(pos)
        if !visited.contains(neighbor) && canTravel(pos, neighbor)
      } yield (neighbor, pos)).toList

      val newVisited = visited ++ (unvisitedNeighbors map { case (neighbor, predecessor) => neighbor})
      val newPredecessors = predecessors ++ Map(unvisitedNeighbors: _*)
      val newQueue = (unvisitedNeighbors map { case (neighbor, predecessor) => neighbor}).toSet

      if ((newQueue intersect goals).nonEmpty) newPredecessors
      else BFS(goals, newQueue.toSet, newVisited, newPredecessors)
    }

    def neighbors(start: Position): List[Position] =
      (for {
        zx <- -1 to 1
        zy <- -1 to 1
        if (zx == 0 || zy == 0) && !(zx == 0 && zy == 0)
      } yield Position(start.x + zx, start.y + zy))
        .filter(p => p.x >= 0 && p.y >= 0 && p.x < board.height && p.y < board.height)
        .toList

    def canTravel(from: Position, to: Position) = {
      if (sameColumn(from, to)) {
        !board.walls.exists(wall => wallHorizontallyBlocks(Position(math.max(from.x, to.x), math.max(from.y, to.y)), wall))
      } else {
        !board.walls.exists(wall => wallVerticallyBlocks(Position(math.max(from.x, to.x), math.max(from.y, from.y)), wall))
      }
    }

    def sameColumn(p1: Position, p2: Position) = p1.x == p2.x

    def wallVerticallyBlocks(p: Position, wall: Wall) = wall.orientation == 'V' && wall.wallX == p.x && wall.wallY <= p.y && wall.wallY + wall.size > p.y

    def wallHorizontallyBlocks(p: Position, wall: Wall) = wall.orientation == 'H' && wall.wallY == p.y && wall.wallX <= p.x && wall.wallX + wall.size > p.x
  }


  def main(args: Array[String]) {
    val Array(w, h, playercount, myid) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
    val players = (for (i <- 0 until playercount) yield {
      PlayerClass(Position(), -1, i)
    }).toList
    val board = Board(players, w, h, List())
    val me = players.find(f => f.id == myid).get
    val pathDeterminer = new PathDeterminer(board)
    // game loop
    while (true) {
      for (i <- 0 until playercount) {
        val Array(x, y, wallsleft) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
        val player = board.players.find(f => f.id == i).get
        player.position = Position(x, y)
        player.wallsLeft = wallsleft
      }
      val wallcount = scala.io.StdIn.readInt() // number of walls on the board
      board.walls = (for (i <- 0 until wallcount) yield {
        val Array(_wallx, _wally, wallorientation) = scala.io.StdIn.readLine split " "
        val wallx = _wallx.toInt
        val wally = _wally.toInt
        Wall(wallx, wally, wallorientation.charAt(0))
      }).toList

      // Write an action using println
      // To debug: Console.err.println("Debug messages...")
      val target = pathDeterminer.shortestPathToFinish(myid).head
      Console.err.println("target: " + target)
      Console.err.println("position: " + me.position)
      if (target.x == me.position.x)
        if (target.y > me.position.y) println("DOWN")
        else println("UP")
      else if (target.x > me.position.x) println("RIGHT")
      else println("LEFT")
    }
  }

}
