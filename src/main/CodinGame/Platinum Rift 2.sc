import scala.collection.immutable.HashMap

class Path(val origin: Zone, val destination: Zone, val moves: List[Move])
class Pod(val owner: Int, var size: Int)
class Zone(val id: Int, val platinumSource: Int, var owner: Int, var occupants: List[Pod], val isHeadquarters: Boolean) {
  override def toString: String = "zone " + id + "; platinum " + platinumSource + "; owner " + owner
}

class BuyCommand(val zone: Zone, val numToBuy: Int)
class Move(val origin: Int, val destination: Int) {
  override def toString: String = "zone " + origin + " -> zone " + destination

  def reverse: Move = new Move(destination, origin)
}


class Board(val zones: HashMap[Int, Zone], val adjacencyList: Map[Int, Set[Int]]) {
  def neighborsWithHistory(path: Path): Stream[Path] = {
    val adjacentZones = adjacencyList.get(path.destination.id).get
    if (adjacentZones.isEmpty) Stream()
    else
      adjacentZones
        .map(zoneId => new Path(path.origin, zones.get(zoneId).get, path.moves ++ List(new Move(path.destination.id, zoneId)) ))
        .toStream
  }

  def getPlayerPodSizeForZone(zoneId: Int, playerId: Int): Int = {
    val pod = zones.get(zoneId).get.occupants.filter(p => p.owner == playerId)
    pod.headOption.map(_.size).getOrElse(0)
  }

  private
  def zoneAdjacentOwnersCheck(p: Int => Boolean)(zoneId: Int, playerId: Int): Boolean = {
    adjacencyList.get(zoneId).get
      .map(zoneId => zones.get(zoneId).get.owner)
      .exists(p)
  }

  def zoneHasAdjacentNotOwnedByPlayer(zoneId: Int, playerId: Int): Boolean =
    zoneAdjacentOwnersCheck(ownerId => ownerId != playerId)(zoneId, playerId)

  def zoneHasAdjacentOwnedByDifferentPlayer(zoneId: Int, playerId: Int): Boolean =
    zoneAdjacentOwnersCheck(ownerId => ownerId != playerId && ownerId != -1)(zoneId, playerId)

  def newNeighborsOnly(neighbors: Stream[Path], explored: Set[Zone]): Stream[Path] =
    neighbors.filter(path => !explored.contains(path.destination))

  def from(startZone: Zone): Stream[Path] = {
    def iter(initial: Stream[Path], explored: Set[Zone]): Stream[Path]={
      if (initial.isEmpty) Stream.empty
      val possibleMoves = for {
        path <- initial
        possibleMove <- newNeighborsOnly(neighborsWithHistory(path), explored)
      } yield possibleMove
      val updatedExplored = explored ++ possibleMoves.map(f => f.destination)
      if (possibleMoves.isEmpty) initial
      else initial #::: iter(possibleMoves.toStream, updatedExplored)
    }

    iter(Stream(new Path(startZone, startZone, List())), Set(startZone))
  }
}

trait MoveStrategy {
  def printMoves(b: Board, player: Int)
}

class Week1And2MoveStrategy extends MoveStrategy {
  def printMoves(b: Board, player: Int) = {
    val zonesWithPlayerPods = b.zones.filter { case (zoneId, zone) => zone.occupants.exists(pod => pod.owner == player)}
    val moves = zonesWithPlayerPods
      .map { case (zoneId, zone) => getTargetMoveZone(b, zone, player)}
      .filter(_.size > 0)
      .map(m => m.head)
    val moveString = moves.foldLeft("") { case (r, path) => r + 1 + " " + path.moves.last.origin + " " + path.moves.last.destination + " "}
    println(moveString)
  }

  private
  def getTargetMoveZone(board: Board, startZone: Zone, player: Int): Stream[Path] = {
    val moves = board.from(startZone)
    val movesWithinX = moves.takeWhile { case path => path.moves.size < 15}
    val sorted = movesWithinX.sortWith { case (pathA, pathB) =>
        pathA.moves.size - pathA.destination.platinumSource > pathB.moves.size - pathB.destination.platinumSource
    }
    sorted.filter(f => f.destination.owner != player)
  }
}


trait BuyStrategy {
  def printBuys(board: Board, player: Int, availablePlatinum: Int) = {
    println("WAIT")
  }
}

class DefaultBuyStrategy extends BuyStrategy
class Week1And2BuyStrategy extends BuyStrategy {
  override def printBuys(board: Board, player: Int, availablePlatinum: Int) = {
    val buys = getBuyTargets(board, player, availablePlatinum)
    val buyString = buys.foldLeft("")((r, c) => r + c.numToBuy + " " + c.zone.id + " ")
    println(buyString)
  }

  private
  def getBuyTargets(board: Board, player: Int, availablePlatinum: Int): List[BuyCommand] = {
    val zonesWithPlayerPods = board.zones.filter { case (zoneId, zone) => zone.occupants.exists(pod => pod.owner == player)}.toList
    val sorted = zonesWithPlayerPods.sortWith { case ((zone1Id, zone1), (zone2Id, zone2)) =>
      board.getPlayerPodSizeForZone(zone1Id, player) < board.getPlayerPodSizeForZone(zone2Id, player)
    }
    val edge = sorted.dropWhile { case (zoneId, zone) => !board.zoneHasAdjacentNotOwnedByPlayer(zoneId, player)}
    val enemyEdge = sorted.dropWhile { case (zoneId, zone) => !board.zoneHasAdjacentOwnedByDifferentPlayer(zoneId, player)}

    if (enemyEdge.nonEmpty) List(new BuyCommand(enemyEdge.head._2, availablePlatinum / 20))
    else if (edge.nonEmpty) List(new BuyCommand(edge.head._2, availablePlatinum / 20))
    else List(new BuyCommand(sorted.head._2, availablePlatinum / 20))
  }
}


//def getAllPaths(board: Board): Promise[HashMap[Zone, Set[Path]]] = {
//  val p = Promise[HashMap[Zone, Set[Path]]]()
//  val priorityTargets = getPriorityTargetsForPlayer(board, 0)
//  val f = Future {
//    val allPaths = for {
//      (startZoneId, startZone) <- board.zones
//    } yield board.from(Stream((startZone, List())), Set())
//
//    val a = for {
//      zoneWithPaths <- allPaths
//      pathCandidate <- zoneWithPaths
//    } yield pathCandidate
//
//    new HashMap[Zone, Set[Path]]
//  }
//
//  f onComplete (x => p.complete(x))
//  p
//}
val zones = List(new Zone(1, 1, -1, List(new Pod(0, 1)), true), new Zone(2, 0, -1, List(), false), new Zone(3, 4, -1, List(), false))
val zm = HashMap(zones.map(m => (m.id, m)): _*)
val adjacencies = HashMap((1, Set(2)), (2, Set(1)), (2, Set(3)), (3, Set(2)))
val board = new Board(zm, adjacencies)
(new DefaultBuyStrategy).printBuys(board, 0, 1)
(new Week1And2MoveStrategy).printMoves(board, 0)
board.getPlayerPodSizeForZone(1, 0)
def getPriorityTargetsForPlayer(board: Board, playerId: Int): List[Zone] = {
  board.zones
    .filter { case (zoneId, zone) => zone.platinumSource > 0 || (zone.isHeadquarters && zone.owner != playerId)}
    .map { case (zoneId, zone) => zone}
    .toList
}