import java.util.concurrent.TimeUnit

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

object Player {

  class Path(val origin: Zone, val destination: Zone, val moves: List[Move])

  class Pod(val owner: Int, var size: Int)

  class BuyCommand(val zone: Zone, val numToBuy: Int)

  class Zone(val id: Int, val platinumSource: Int, var owner: Int, var occupants: List[Pod], var isHeadquarters: Boolean = false)

  class Move(val origin: Int, val destination: Int) {
    override def toString: String = "zone " + origin + " -> zone " + destination
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


  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {
    def all[T](fs: List[Future[T]]): Future[List[T]] = {
      val successful = Promise[List[T]]()
      successful.success(Nil)
      fs.foldRight(successful.future) {
        (f, acc) => for {x <- f; xs <- acc} yield x :: xs
      }
    }

    def delay(t: Duration): Future[Unit] = {
      val p = Promise[Unit]()
      Future {
        blocking(Thread.sleep(t.toMillis))
        p complete Try(Unit)
      }
      p.future
    }

    def any[T](fs: List[Future[T]]): Future[T] = {
      val p = Promise[T]()
      fs.foreach(f => f onComplete p.tryComplete)
      p.future
    }
  }


  trait MoveStrategy {
    def printMoves(maxComputeTime: Duration) = print("WAIT")
  }

  class DefaultMoveStrategy extends MoveStrategy


  class BFSMoveStrategy(board: Board, player: Int) extends MoveStrategy {
    var pathCache = new HashMap[Zone, List[Path]]
    var timedOutWhileGettingPaths = List[Zone]()
    var distanceBetweenHeadquarters = 9999

    override def printMoves(maxComputeTime: Duration) = {
      val startTime = System.currentTimeMillis
      val zonesWithPlayerPods = board.zonesOwnedByPlayer(player)
      zonesWithPlayerPods
        .toList
        .sortBy(z => !pathCache.contains(z))
        .toIterator
        .takeWhile(_ => System.currentTimeMillis - startTime < maxComputeTime.toMillis)
        .foreach { zone => printMoveForZone(zone, Duration(maxComputeTime.toMillis - (System.currentTimeMillis - startTime), TimeUnit.MILLISECONDS))}
      println("")
    }

    private
    def printMoveForZone(zone: Zone, maxComputeTime: Duration) = {
      val podsInZone = board.getPlayerPodSizeForZone(zone.id, player)
      try {
        val paths = Await.result(getPathsForZone(zone), maxComputeTime).filter(f => f.moves.size > 0).take(podsInZone)
        val podsToMove = (podsInZone / paths.size + 0.5).toInt
        for (path <- paths) {
          print(podsToMove + " " + path.moves.head.origin + " " + path.moves.head.destination + " ")
        }
      } catch {
        case _: Throwable => timedOutWhileGettingPaths = zone :: timedOutWhileGettingPaths
      }
    }

    private
    def getPathsForZone(startZone: Zone): Future[List[Path]] = {
      val p = Promise[List[Path]]()
      Future {
        val startTime = System.currentTimeMillis()
        if (!pathCache.get(startZone).isDefined && !timedOutWhileGettingPaths.contains(startZone)) {
          val moves = board.from(startZone).toIterator
          val movesWithinX = moves.takeWhile { case path => System.currentTimeMillis - startTime < 20 && path.moves.size < 40}
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
      val priorityZones = unowned.filter(path => path.destination.platinumSource > 0)
      if (shouldTargetHeadquarters(unowned))
        unowned.find(path => path.destination.isHeadquarters) match {
          case Some(s) => List(s)
          case _ => priorityZones
        }
      else if (priorityZones.size > 0)
        priorityZones.toList
      else
        unowned.toList
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
      val enemyIsWithin7Moves = {
        val pathToHeadquartersMonad = pathsToUnownedZones.find(path => path.destination.isHeadquarters)
        pathToHeadquartersMonad match {
          case Some(path) => path.moves.size < 7
          case _ => false
        }
      }

      (_ICanProduceMorePodsNextTurn && enemyIsWithin7Moves) || distanceBetweenHeadquarters < 9
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

  def main(args: Array[String]) {
    // playercount: the amount of players (2 to 4)
    // myid: my player ID (0, 1, 2 or 3)
    // zonecount: the amount of zones on the map
    // linkcount: the amount of links between all zones
    val Array(playercount, myid, zonecount, linkcount) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
    val zones = for (i <- 0 until zonecount) yield {
      // zoneid: this zone's ID (between 0 and zoneCount-1)
      // platinumsource: the amount of Platinum this zone can provide per game turn
      val Array(zoneid, platinumsource) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
      new Zone(zoneid, platinumsource, -1, List())
    }
    val zoneMap = HashMap(zones.map(m => (m.id, m)): _*)
    val adjacentTouples = for (i <- 0 until linkcount) yield {
      val Array(zone1, zone2) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
      List((zone1, zone2), (zone2, zone1))
    }
    val adjacencyList = adjacentTouples
      .flatten
      .groupBy(p => p._1)
      .map { case (k, v) => (k, v.map(_._2).toSet)}

    val board = new Board(zoneMap, adjacencyList)
    val bfsMoveStrategy = new BFSMoveStrategy(board, myid)
    // game loop
    var round1 = true
    var test: Iterable[Path] = Nil
    while (true) {
      val startTime = System.currentTimeMillis()
      val platinum = readInt // my available Platinum
      for (i <- 0 until zonecount) {
        // zid: this zone's ID
        // ownerid: the player who owns this zone (-1 otherwise)
        // podsp0: player 0's PODs on this zone
        // podsp1: player 1's PODs on this zone
        // podsp2: player 2's PODs on this zone (always 0 for a two player game)
        // podsp3: player 3's PODs on this zone (always 0 for a two or three player game)
        val Array(zid, ownerid, podsp0, podsp1, podsp2, podsp3) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
        val pods = List(new Pod(0, podsp0), new Pod(1, podsp1), new Pod(2, podsp2), new Pod(3, podsp3))
          .filter(f => f.size > 0)
        val zone = board.zones.get(zid).get
        zone.occupants = pods
        zone.owner = ownerid
        if (round1 && ownerid != -1) {
          zone.isHeadquarters = true
        }
      }
      val readTime = System.currentTimeMillis() - startTime
      Console.err.println(s"time after reading board: $readTime")

      val maxWait = Duration(90 - (System.currentTimeMillis - startTime), TimeUnit.MILLISECONDS)
      bfsMoveStrategy.printMoves(maxWait)
      val moveTime = System.currentTimeMillis() - startTime
      Console.err.println(s"Time after calculating moves: $moveTime")

      (new DefaultBuyStrategy).printBuys(board, myid, platinum)
      round1 = false
    }
  }
}