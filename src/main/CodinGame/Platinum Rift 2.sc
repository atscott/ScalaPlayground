
import java.util.concurrent.TimeUnit

import scala.collection.immutable.HashMap
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise}
import scala.language.postfixOps
import scala.util.Try

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
  def printMoves(maxComputeTime: Duration): Future[Unit] = {
    print("WAIT"); Future {}
  }
}

class DefaultMoveStrategy extends MoveStrategy

class BFSMoveStrategy(board: Board, player: Int) extends MoveStrategy {
  var pathCache = new HashMap[Zone, List[Path]]
  var timedOutWhileGettingPaths = List[Zone]()
  var distanceBetweenHeadquarters = 9999

  override def printMoves(maxComputeTime: Duration): Future[Unit] = {
    val startTime = System.currentTimeMillis
    val zonesWithPlayerPods = board.zonesOwnedByPlayer(player)
    val futures = zonesWithPlayerPods
      .map { zone => printMoveForZone(zone, Duration(maxComputeTime.toMillis - (System.currentTimeMillis - startTime), TimeUnit.MILLISECONDS))}

    val p = Promise[Unit]()
    all(futures.toList).onComplete(_ => p complete Try())
    p.future
  }

  private
  def printMoveForZone(zone: Zone, maxComputeTime: Duration): Future[Unit] = {
    val p = Promise[Unit]()
    val startTime = System.currentTimeMillis
    val podsInZone = board.getPlayerPodSizeForZone(zone.id, player)
    getPathsForZone(zone) onComplete (r => {
      if (r.isSuccess && System.currentTimeMillis - startTime < maxComputeTime.toMillis)
      {
        val paths = r.get
        val filteredPaths = paths.filter(f => f.moves.size > 0).take(podsInZone)
        val podsToMove = (podsInZone / filteredPaths.size + 0.5).toInt
        for (path <- filteredPaths) {
          if (path.destination.isHeadquarters) {
            print(podsToMove + " " + path.moves.head.origin + " " + path.moves.head.destination + " ")
          } else {
            print(podsToMove + " " + path.moves.head.origin + " " + path.moves.head.destination + " ")
          }
        }
      }
      p complete Try()
    })
    p.future
  }

  private
  def getPathsForZone(startZone: Zone): Future[List[Path]] = {
    val p = Promise[List[Path]]()
    Future {
      val startTime = System.currentTimeMillis()
      if (!pathCache.get(startZone).isDefined && !timedOutWhileGettingPaths.contains(startZone)) {
        val moves = board.from(startZone).toIterator
        val movesWithinX = moves.takeWhile { case path => System.currentTimeMillis - startTime < 20}
          .toList
        val sorted = movesWithinX.sortWith { case (pathA, pathB) =>
          pathA.moves.size - pathA.destination.platinumSource < pathB.moves.size - pathB.destination.platinumSource
        }
        pathCache = pathCache.updated(startZone, sorted.toList)
        timedOutWhileGettingPaths = timedOutWhileGettingPaths.filter(f => f != startZone)
      }
      p complete Try(prioritizePaths(pathCache.get(startZone).get))
    }
    p.future
  }

  private
  def prioritizePaths(paths: List[Path]): List[Path] = {
    val unowned = paths.filter(f => f.destination.owner != player)
    if (shouldTargetHeadquarters(unowned)) {
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
    if (distanceBetweenHeadquarters == 9999) {
      if (pathsToUnownedZones.head.origin.isHeadquarters) {
        for (path <- pathsToUnownedZones.find(path => path.destination.isHeadquarters)) {
          distanceBetweenHeadquarters = path.moves.size
        }
      }
    }
    val myProduction = board.playerPlatinumProduction(player)
    val otherProduction = board.playerPlatinumProduction(1)
    val _ICanProduceMorePodsNextTurn = myProduction - otherProduction > 20
    val _IAmDominating = myProduction - otherProduction > 30
    val enemyIsWithin8Moves = {
      val pathToHeadquartersMonad = pathsToUnownedZones.find(path => path.destination.isHeadquarters)
      pathToHeadquartersMonad match {
        case Some(path) => path.moves.size < 9
        case _ => false
      }
    }

    (_ICanProduceMorePodsNextTurn && enemyIsWithin8Moves) || _IAmDominating || distanceBetweenHeadquarters < 9
  }

  def all[T](fs: List[Future[T]]): Future[List[T]] = {
    val successful = Promise[List[T]]()
    successful.success(Nil)
    fs.foldRight(successful.future) {
      (f, acc) => for {x <- f; xs <- acc} yield x :: xs
    }
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
