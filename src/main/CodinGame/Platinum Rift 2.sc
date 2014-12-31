
import java.util.concurrent.TimeUnit

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.language.postfixOps

class Path(val origin: Zone, val destination: Zone, val moves: List[Move])

class Pod(val owner: Int, var size: Int)

class Zone(val id: Int, val platinumSource: Int, var owner: Int, var occupants: List[Pod], val isHeadquarters: Boolean = false) {
  override def toString: String = "zone " + id + "; platinum " + platinumSource + "; owner " + owner
}

class BuyCommand(val zone: Zone, val numToBuy: Int)

class Move(val origin: Int, val destination: Int) {
  override def toString: String = "zone " + origin + " -> zone " + destination

  def reverse: Move = new Move(destination, origin)
}


class Board(val zones: HashMap[Int, Zone], val adjacencyList: Map[Int, Set[Int]]) {
  def zonesOwnedByPlayer(playerId: Int): Iterable[Zone] =
    zones.filter { case (zoneId, zone) => zone.occupants.exists(pod => pod.owner == playerId)}.map(m => m._2)

  def playerPlatinumProduction(playerId: Int): Int =
    zonesOwnedByPlayer(playerId).foldRight(0) { case (zone, acc) => zone.platinumSource + acc}

  def getPlayerPodSizeForZone(zoneId: Int, playerId: Int): Int = {
    val pod = zones.get(zoneId).get.occupants.filter(p => p.owner == playerId)
    pod.headOption.map(_.size).getOrElse(0)
  }

  def zoneHasAdjacentNotOwnedByPlayer(zoneId: Int, playerId: Int): Boolean =
    zoneAdjacentOwnersCheck(ownerId => ownerId != playerId)(zoneId, playerId)

  def zoneHasAdjacentOwnedByDifferentPlayer(zoneId: Int, playerId: Int): Boolean =
    zoneAdjacentOwnersCheck(ownerId => ownerId != playerId && ownerId != -1)(zoneId, playerId)

  private
  def zoneAdjacentOwnersCheck(p: Int => Boolean)(zoneId: Int, playerId: Int): Boolean = {
    adjacencyList.get(zoneId).get
      .map(zoneId => zones.get(zoneId).get.owner)
      .exists(p)
  }

  def from(startZone: Zone): Stream[Path] = {
    def iter(initial: Stream[Path], visitQueue: List[Path], explored: Set[Zone]): Stream[Path] = {
      if (visitQueue.isEmpty) initial
      else {
        val possibleMoves = for {
          path <- visitQueue
          possibleMove <- newNeighborsOnly(neighborsWithHistory(path), explored)
        } yield possibleMove
        val filteredVisitQueue = visitQueue.filter(path => !explored.contains(path.destination))
        val updatedExplored = explored ++ visitQueue.map(path => path.destination)
        val uniqueMoves = possibleMoves.groupBy(g => g.destination).map(m => m._2.head).toList
        initial #::: iter(filteredVisitQueue.toStream, uniqueMoves, updatedExplored)
      }
    }

    iter(List().toStream, List(new Path(startZone, startZone, List())), Set(startZone))
  }

  def neighborsWithHistory(path: Path): Stream[Path] = {
    val adjacentZones = adjacencyList.get(path.destination.id).get
    if (adjacentZones.isEmpty) Stream()
    else
      adjacentZones
        .map(zoneId => new Path(path.origin, zones.get(zoneId).get, path.moves ++ List(new Move(path.destination.id, zoneId))))
        .toStream
  }

  def newNeighborsOnly(neighbors: Stream[Path], explored: Set[Zone]): Stream[Path] =
    neighbors.filter(path => !explored.contains(path.destination))
}

trait MoveStrategy {
  def printMoves(maxComputeTime: Duration) = print("WAIT")
}

class DefaultMoveStrategy extends MoveStrategy


class BFSMoveStrategy(board: Board, player: Int) extends MoveStrategy {
  var pathCache = new HashMap[Zone, List[Path]]

  override def printMoves(maxComputeTime: Duration) = {
    val startTime = System.currentTimeMillis
    val zonesWithPlayerPods = board.zonesOwnedByPlayer(player)
    zonesWithPlayerPods
      .toIterator
      .takeWhile(_ => System.currentTimeMillis - startTime < maxComputeTime.toMillis)
      .foreach { zone => printMoveForZone(zone)}
    println("")
  }

  private
  def printMoveForZone(zone: Zone) = {
    val pathMonad = getPathsForZone(zone).find(f => f.moves.size > 0).headOption
    for( path <- pathMonad) {
      if(path.destination.isHeadquarters){
        val podsToMove = board.getPlayerPodSizeForZone(zone.id, player)
        print(podsToMove + " " + path.moves.head.origin + " " + path.moves.head.destination + " ")
      }else{
        val podsToMove = new HalfPodMoveStrategy(board, player).getNumToMove(zone)
        print(podsToMove + " " + path.moves.head.origin + " " + path.moves.head.destination + " ")
      }
    }
  }

  private
  def getPathsForZone(startZone: Zone): List[Path] = {
    val startTime = System.currentTimeMillis()
    if (!pathCache.get(startZone).isDefined) {
      val moves = board.from(startZone).toIterator
      val movesWithinX = moves.takeWhile { case path => System.currentTimeMillis - startTime < 20 && path.moves.size < 40}
        .toList
      val sorted = movesWithinX.sortWith { case (pathA, pathB) =>
        pathA.moves.size - pathA.destination.platinumSource < pathB.moves.size - pathB.destination.platinumSource
      }
      pathCache = pathCache.updated(startZone, sorted.toList)
    }
    prioritizePaths(pathCache.get(startZone).get)
  }

  private
  def prioritizePaths(paths: List[Path]): List[Path] = {
    val unowned = paths.filter(f => f.destination.owner != player)
    if(shouldTargetHeadquarters(unowned)){
      unowned.filter(path => path.destination.isHeadquarters)
    }
    else if (unowned.exists(path => path.destination.platinumSource > 0 || path.destination.isHeadquarters)) {
      unowned.filter(path => path.destination.platinumSource > 0 || path.destination.isHeadquarters).toList
    }
    else {
      unowned.toList
    }
  }

  private
  def shouldTargetHeadquarters(pathsToUnownedZones: List[Path]): Boolean = {
    val myProduction = board.playerPlatinumProduction(player)
    val otherProduction = board.playerPlatinumProduction(1)
    val _ICanProduceMorePodsNextTurn = myProduction - otherProduction > 20
    val _IAmDominating = myProduction - otherProduction > 40
    val enemyIsWithin8Moves = {
      val pathToHeadquartersMonad = pathsToUnownedZones.find(path => path.destination.isHeadquarters)
      pathToHeadquartersMonad match {
        case Some(path) => path.moves.size < 9
        case _ => false
      }
    }

    (_ICanProduceMorePodsNextTurn && enemyIsWithin8Moves) || _IAmDominating
  }
}

class PriorityMoveStrategy(board: Board, playerId: Int) extends MoveStrategy {
  private var isReady = false
  var zoneMoveMap = new HashMap[Zone, List[Path]]

  def ready() = isReady

  override def printMoves(maxComputeTime: Duration): Unit = {
    if (!isReady) return

  }

  def prepare(initialZone: Zone): Unit = {
    getPriorityPathsForZone(initialZone) onComplete (r => {
      isReady = true
      val thingies = r.get
      thingies.foreach(path => {
        var intermediateMoves = path.moves
        do {
          val origin = board.zones.get(intermediateMoves.head.origin).get
          val pathsForZone = zoneMoveMap.get(origin)
          val newPath = new Path(origin, path.destination, intermediateMoves)
          if (pathsForZone.isDefined) zoneMoveMap = zoneMoveMap.updated(origin, newPath :: pathsForZone.get)
          else zoneMoveMap = zoneMoveMap.updated(origin, List(newPath))

          intermediateMoves = intermediateMoves.tail
        } while (intermediateMoves.size > 0)
      })
      Console.err.println(r.get.size)
    })
  }

  private
  def getPriorityPathsForZone(zone: Zone): Future[Iterable[Path]] = {
    val p = Promise[Iterable[Path]]()
    val priorityTargets = getPriorityTargetsForPlayer
    val f = Future {
      val allPathsForZone = board.from(zone)

      val pathsToPriorityTargets = for {
        singlePath <- allPathsForZone
        if priorityTargets.contains(singlePath.destination)
      } yield singlePath
      pathsToPriorityTargets.groupBy(path => path.destination).map { case (destination, paths) => paths.sortWith { case (pathA, pathB) => pathA.moves.size < pathB.moves.size}.head}
    }

    f onComplete (x => p.complete(x))
    p.future
  }

  def getPriorityTargetsForPlayer: List[Zone] = {
    board.zones
      .filter { case (zoneId, zone) => zone.platinumSource > 0 || (zone.isHeadquarters && zone.owner != playerId)}
      .map { case (zoneId, zone) => zone}
      .toList
  }
}


trait NumPodsToMoveStrategy {
  def getNumToMove(zone: Zone): Int
}

class HalfPodMoveStrategy(board: Board, player: Int) extends NumPodsToMoveStrategy {
  def getNumToMove(zone: Zone): Int = {
    val myPodsInZone = board.getPlayerPodSizeForZone(zone.id, player)
    if (zone.isHeadquarters)
      if (myPodsInZone > 4)
        myPodsInZone - 4
      else
        0
    else {
      (myPodsInZone / 2.0 + 0.5).toInt
    }
  }
}

trait BuyStrategy {
  def printBuys(board: Board, player: Int, availablePlatinum: Int) = println("WAIT")
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


val zones = List(new Zone(1, 1, -1, List(new Pod(0, 1)), true), new Zone(2, 0, -1, List(), false), new Zone(3, 4, -1, List(), false))
val zm = HashMap(zones.map(m => (m.id, m)): _*)
val adjacencies = HashMap((1, Set(2)), (2, Set(1)), (2, Set(3)), (3, Set(2)))
val board = new Board(zm, adjacencies)
(new DefaultBuyStrategy).printBuys(board, 0, 1)
new BFSMoveStrategy(board, 0).printMoves(Duration(1000, TimeUnit.MILLISECONDS))
board.getPlayerPodSizeForZone(1, 0)

new BFSMoveStrategy(board, 0).printMoves(Duration(1000, TimeUnit.MILLISECONDS))
