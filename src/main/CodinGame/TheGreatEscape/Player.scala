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
    def getWalltoBlock(player: PlayerClass, theirShortestPath: List[Position]): Option[Wall] = {
      val wallCandidates = for {
        i <- 0 until theirShortestPath.size
        offset <- 0 until defaultWallSize
      } yield {
        val target = theirShortestPath(i)
        val previous = if (i == 0) player.position else theirShortestPath(i - 1)
        val moveDir = moveDirection(previous, target)


        val wallX =
          if (moveDir == 'L') previous.x
          else if (moveDir == 'R') target.x
          else target.x - offset

        val wallY =
          if (moveDir == 'U') previous.y
          else if (moveDir == 'D') target.y
          else target.y - offset

        val wallDir = if (moveDir == 'U' || moveDir == 'D') 'H' else 'V'

        val wall = Wall(wallX, wallY, wallDir)

        val valid = wallIsValid(wall, board)
        lazy val redirectionCost = callRedirectionPathDifference(wall, board, theirShortestPath.size, player)
        if (valid && (redirectionCost > 1 || theirShortestPath.size < 2))
          Some((wall, redirectionCost))
        else
          None
      }
      val filtered = wallCandidates
        .flatten
        .toList
        .sortBy { case (wall, cost) => -1 * cost}
      filtered
        .headOption
        .map { case (wall, cost) => wall}
    }

    private def moveDirection(start: Position, finish: Position) =
      if (start.x != finish.x)
        if (start.x > finish.x) 'L'
        else 'R'
      else if (start.y > finish.y) 'U'
      else 'D'

    def wallIsValid(w: Wall, b: Board) =
      !board.walls.exists(boardWall => {fullyIntersect(boardWall.p1, boardWall.p2, w.p1, w.p2)}) && wallIsWithinBounds(w, b)


    def wallIsWithinBounds(w: Wall, b: Board) =
      w.p1.x >= 0 && w.p2.x >= 0 && w.p1.x <= b.width && w.p2.x <= b.width &&
        w.p1.y >= 0 && w.p2.y >= 0 && w.p1.y <= b.height && w.p2.y <= b.height

    def fullyIntersect(p1: Position, q1: Position, p2: Position, q2: Position): Boolean = {
      def inLine(A: Position, B: Position, C: Position) =
        if (A.x == C.x) B.x == C.x
        else if (A.y == C.y) B.y == C.y
        else (A.x - C.x) * (A.y - C.y) == (C.x - B.x) * (C.y - B.y)

      lazy val parallel = {
        val slope1 = Try((p1.y - q1.y) / (p1.x - q1.x))
        val slope2 = Try((p2.y - q2.y) / (p2.x - q2.x))
        (slope1, slope2) match {
          case (Success(s1), Success(s2)) => s1 == s2
          case (Failure(_), Failure(_)) => true
          case _ => false
        }
      }

      lazy val shareOnlyOnePoint = List(p1, q1, p2, q2).toSet.size == 3

      if (!doIntersect(p1, q1, p2, q2)) false
      else
      if (parallel) !shareOnlyOnePoint
      else !(inLine(p1, q1, p2) || inLine(p1, q1, q2))
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

    def callRedirectionPathDifference(w: Wall, b: Board, currentDistance: Int, player: PlayerClass): Int = {
      val newBoard = Board(List(player), b.width, b.height, w :: b.walls)
      val pathDeterminer = new PathDeterminer(newBoard)
      val newPath = pathDeterminer.shortestPathToFinish(player.id)
      newPath match {
        case Some(p) => p.size - currentDistance
        case None => -1
      }
    }

  }

  class PathDeterminer(board: Board) {

    def getPathFromPredecessorMap(start: Position, end: Position, predecessors: Map[Position, Position]): List[Position] = {
      if (!predecessors.get(end).isDefined || end == start) List()
      else getPathFromPredecessorMap(start, predecessors.get(end).get, predecessors) ++ List(end)
    }

    def shortestPathToFinish(playerId: Int): Option[List[Position]] = {
      val targets = if (playerId == 0) for (y <- 0 until board.height) yield Position(board.width - 1, y)
      else if (playerId == 1) for (y <- 0 until board.height) yield Position(0, y)
      else if (playerId == 2) for (x <- 0 until board.width) yield Position(x, board.height - 1)
      else for (x <- 0 until board.width) yield Position(x, 0)

      val start = board.players.filter(f => f.id == playerId).head.position
      val predecessors = BFS(targets.toSet, Set(start), Set(start), Map())
      predecessors.find { case (pos, _) => targets.contains(pos)} match {
        case Some((pos, _)) => Some(getPathFromPredecessorMap(start, pos, predecessors))
        case _ => None
      }
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
        !board.walls.exists(wall => wallHorizontallyBlocks(from, to, wall))
      } else {
        !board.walls.exists(wall => wallVerticallyBlocks(from, to, wall))
      }
    }

    def sameColumn(p1: Position, p2: Position) = p1.x == p2.x

    def wallVerticallyBlocks(from: Position, to: Position, wall: Wall) =
      wall.orientation == 'V' &&
        wall.wallX == math.max(from.x, to.x) &&
        wall.wallY <= from.y &&
        wall.wallY + wall.size > from.y

    def wallHorizontallyBlocks(from: Position, to: Position, wall: Wall) =
      wall.orientation == 'H' &&
        wall.wallY == math.max(from.y, to.y) &&
        wall.wallX <= from.x &&
        wall.wallX + wall.size > from.x
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

      val paths = (for {
        playerId <- 0 until playercount
        player <- board.players.filter(f => f.id == playerId && f.wallsLeft != -1)
      } yield {
        pathDeterminer.shortestPathToFinish(player.id) match{
          case Some(path) => Some((player, path))
          case _ => None
        }
      }).flatten.toList
      val closestToWinning = paths.groupBy { case (playerId, path) => path.size}.minBy(m => m._1)._2

      closestToWinning.find { case (player, path) => player.id == myid} match {
        case Some((_, path)) =>
          printMove(path.head)
        case _ =>
          if (board.players.find(f => f.id == myid).get.wallsLeft > 0) {
            val wallOption = wallDeterminer.getWalltoBlock(closestToWinning.head._1, closestToWinning.head._2)
            wallOption match {
              case Some(wall) => println(wall.wallX + " " + wall.wallY + " " + wall.orientation + " I'M GONNA LOSE")
              case _ => printMove(paths.find { case (player, path) => player.id == myid}.get._2.head, "I'M GONNA LOSE")
            }
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
