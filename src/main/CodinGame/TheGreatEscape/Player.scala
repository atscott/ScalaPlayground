package CodinGame.TheGreatEscape

import scala.util.{Success, Failure, Try}

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player {
  val defaultWallSize = 2

  case class Position(x: Int = -1, y: Int = -1)

  case class PlayerClass(var position: Position, var wallsLeft: Int, id: Int)

  case class Board(players: List[PlayerClass], width: Int, height: Int, var walls: List[Wall])

  case class Wall(wallX: Int, wallY: Int, orientation: Char, size: Int = 2) {
    val endX = if (orientation == 'V') wallX else wallX + size
    val endY = if (orientation == 'H') wallY else wallY + size
    val p1 = Position(wallX, wallY)
    val p2 = Position(endX, endY)
  }

  class WallDeterminer(board: Board) {
    def getWalltoBlock(player: PlayerClass, theirShortestPath: List[Position]): Wall = {
      val wallCandidates = for {
        target <- theirShortestPath
        offset <- 0 until defaultWallSize
      } yield {
        val wallDir =
          if (player.position.x == theirShortestPath.head.x) 'H'
          else 'V'

        val wall = if (wallDir == 'V') Wall(target.x, target.y - offset, wallDir)
        else Wall(target.x - offset, target.y, wallDir)
        if (board.walls.exists(boardWall => {
          fullyIntersect(boardWall.p1, boardWall.p2, wall.p1, wall.p2)
        }))
          None
        else
          Some(wall)
      }
      wallCandidates.dropWhile(w => !w.isDefined).head.get
    }


    def fullyIntersect(p1: Position, q1: Position, p2: Position, q2: Position): Boolean = {
      def parallel(p1: Position, q1: Position, p2: Position, q2: Position): Boolean = {
        val slope1 = Try((p1.y - q1.y) / (p1.x - q1.x))
        val slope2 = Try((p2.y - q2.y) / (p2.x - q2.x))
        (slope1, slope2) match {
          case (Success(s1), Success(s2)) => s1 == s2
          case (Failure(_), Failure(_)) => true
          case _ => false
        }
      }
      def inLine(A: Position, B: Position, C: Position) =
        if (A.x == C.x) B.x == C.x
        else if (A.y == C.y) B.y == C.y
        else (A.x - C.x) * (A.y - C.y) == (C.x - B.x) * (C.y - B.y)

      if (!doIntersect(p1, q1, p2, q2)) false
      else {
        if (parallel(p1, q1, p2, q2))
          !((q1.x == p2.x && q1.y == p2.y) || (p1.x == q1.x && p1.y == q1.y))
        else
          !(inLine(p1, q1, p2) || inLine(p1, q1, q2))
      }
    }


    def doIntersect(p1: Position, q1: Position, p2: Position, q2: Position): Boolean = {
      def onSegment(p: Position, q: Position, r: Position): Boolean = {
        if (q.x <= math.max(p.x, r.x) && q.x >= math.min(p.x, r.x) &&
          q.y <= math.max(p.y, r.y) && q.y >= math.min(p.y, r.y))
          true
        else false
      }

      def orientation(p: Position, q: Position, r: Position): Int = {
        val value = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
        if (value == 0) 0
        else if (value > 0) 1
        else 2
      }

      val o1 = orientation(p1, q1, p2)
      val o2 = orientation(p1, q1, q2)
      val o3 = orientation(p2, q2, p1)
      val o4 = orientation(p2, q2, q1)

      if (o1 != o2 && o3 != o4) true
      else if (o1 == 0 && onSegment(p1, p2, q1)) true
      else if (o2 == 0 && onSegment(p1, q2, q1)) true
      else if (o3 == 0 && onSegment(p2, p1, q2)) true
      else if (o4 == 0 && onSegment(p2, q1, q2)) true
      else false
    }

  }

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
    val wallDeterminer = new WallDeterminer(board)
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
      val paths = (for {
        playerId <- 0 until playercount
        player <- board.players.filter(f => f.id == playerId && f.wallsLeft != -1)
      } yield (player, pathDeterminer.shortestPathToFinish(player.id))).toList
      val closestToWinning = paths.groupBy { case (playerId, path) => path.size}.minBy(m => m._1)._2

      closestToWinning.find { case (player, path) => player.id == myid} match {
        case Some((_, path)) =>
          printMove(path.head)
        case _ =>
          if (board.players.find(f => f.id == myid).get.wallsLeft > 0) {
            val wall = wallDeterminer.getWalltoBlock(closestToWinning.head._1, closestToWinning.head._2)
            println(wall.wallX + " " + wall.wallY + " " + wall.orientation + " I'M GONNA LOSE")
          } else {
            printMove(paths.find { case (player, path) => player.id == myid}.get._2.head, "I'M GONNA LOSE")
          }
      }

      def printMove(target: Position, message: String = "I'M GONNA WIN!"): Unit = {
        if (target.x == me.position.x)
          if (target.y > me.position.y) println("DOWN " + message)
          else println("UP " + message)
        else if (target.x > me.position.x) println("RIGHT " + message)
        else println("LEFT " + message)
      }

    }
  }

}
