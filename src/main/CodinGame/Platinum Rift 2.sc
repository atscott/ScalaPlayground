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

  class Zone(val id: Int, var platinumSource: Int, var owner: Int, var occupants: List[Pod], var isHeadquarters: Boolean = false, var turnLastSeen: Int = 0)

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

    def getMaxEnemyPodSizeForZone(zoneId: Int, playerId: Int): Int = {
      zones.get(zoneId).get
        .occupants
        .filter(pod => pod.owner != playerId)
        .foldRight(0) { case (pod, max) => math.max(pod.size, max)}
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

    def numberOfEnemyPodsInPositionToTakeZone(zoneId: Int, playerId: Int): Int =
      (for {
        zone <- adjacentZones(zoneId)
        if zone.owner != playerId
      } yield {
        zone.occupants
          .filter(o => o.owner == zone.owner)
          .foldRight(0)(_.size + _)
      }).sum

    def adjacentZones(zoneId: Int): Iterable[Zone] =
      for {
        id <- adjacencyList.get(zoneId).get
        zone <- zones.get(id)
      } yield zone

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
    def printMoves(maxComputeTime: Duration) = println("WAIT")
  }

  class DefaultMoveStrategy extends MoveStrategy


  class BFSMoveStrategy(board: Board, player: Int) extends MoveStrategy {
    var pathCache = new HashMap[Zone, List[Path]]
    var distanceBetweenHeadquarters = 9999
    var lastUsedPaths = new HashMap[Zone, List[Path]]
    var zonesWithPathsBeingComputed: List[Zone] = Nil

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
        var paths = Await.result(getPathsForZone(zone), maxComputeTime).filter(f => f.moves.size > 0).take(podsInZone)
        if (paths.isEmpty)
          paths = lastUsedPaths.getOrElse(zone, List())
        lastUsedPaths = lastUsedPaths.updated(zone, paths)
        val podsToMove = (podsInZone / paths.size + 0.5).toInt
        for (path <- paths) {
          print(podsToMove + " " + path.moves.head.origin + " " + path.moves.head.destination + " ")
        }

      } catch {
        case _: Throwable =>
      }
    }

    private
    def getPathsForZone(startZone: Zone): Future[List[Path]] = {
      val p = Promise[List[Path]]()
      if (zonesWithPathsBeingComputed.contains(startZone)) p complete Try(List())
      else {
        Future {
          zonesWithPathsBeingComputed = startZone :: zonesWithPathsBeingComputed
          if (!pathCache.get(startZone).isDefined) {
            val moves = board.from(startZone).toIterator
            val movesWithinX = moves.takeWhile { case path => path.moves.size < 30}
              .toList
            pathCache = pathCache.updated(startZone, movesWithinX.toList)

          }
          zonesWithPathsBeingComputed = zonesWithPathsBeingComputed.filter(z => z.id != startZone.id)


          p complete Try(prioritizePaths(pathCache.get(startZone).get))
        }
      }
      p.future
    }


    private
    def prioritizePaths(paths: List[Path]): List[Path] = {
      lazy val shouldDefend = {
        lazy val zone = paths.head.origin
        lazy val onlyOnePod = board.getPlayerPodSizeForZone(zone.id, player) == 1
        lazy val adjacentEnemies = board.numberOfEnemyPodsInPositionToTakeZone(zone.id, player)
        paths.nonEmpty && zone.platinumSource > 0 && onlyOnePod && adjacentEnemies == 1
      }

      lazy val shouldRetreat: Boolean = {
        val zone = paths.head.origin
        lazy val platinum = zone.platinumSource
        lazy val myPodSize = board.getPlayerPodSizeForZone(zone.id, player)
        lazy val enemyPodSize = board.getMaxEnemyPodSizeForZone(zone.id, player)
        (platinum == 0 && enemyPodSize > 0) || myPodSize < enemyPodSize
      }

      lazy val retreatPaths: List[Path] = {
        val uncontestedAndUnowned = paths.filter(f => f.destination.owner == -1)
        if (uncontestedAndUnowned.nonEmpty) uncontestedAndUnowned
        else paths
      }

      lazy val bestPaths: List[Path] = {
        val unowned = paths.filter(f => f.destination.owner != player)
        val priorityZones = unowned.filter(path => path.destination.platinumSource > 0)
        if (shouldTargetHeadquarters(unowned))
          unowned.find(path => path.destination.isHeadquarters) match {
            case Some(s) => List(s)
            case _ => priorityZones
          }
        else if (priorityZones.nonEmpty)
          priorityZones.toList
        else
          unowned.toList
      }

      if (shouldDefend) List()
      else if (shouldRetreat) retreatPaths
      else bestPaths
    }

    private
    def shouldTargetHeadquarters(pathsToUnownedZones: List[Path]): Boolean = {
      def determineHeadquarterDistance() = {
        for (path <- pathsToUnownedZones.find(path => path.destination.isHeadquarters)) {
          distanceBetweenHeadquarters = path.moves.size
        }
      }
      val headquarterDistanceUndetermined = distanceBetweenHeadquarters == 9999
      lazy val startZoneIsHeadquarters = pathsToUnownedZones.head.origin.isHeadquarters

      if (headquarterDistanceUndetermined && startZoneIsHeadquarters) determineHeadquarterDistance()

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
      new Zone(zoneid, 1, -1, List())
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
    var currentTurn = 0
    while (true) {
      val startTime = System.currentTimeMillis()
      val availablePlatinum = readInt // my available Platinum
      for (i <- 0 until zonecount) {
        // zid: this zone's ID
        // ownerid: the player who owns this zone (-1 otherwise)
        // podsp0: player 0's PODs on this zone
        // podsp1: player 1's PODs on this zone
        // podsp2: player 2's PODs on this zone (always 0 for a two player game)
        // podsp3: player 3's PODs on this zone (always 0 for a two or three player game)
        val Array(zid, ownerid, podsp0, podsp1, visible, platinum) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
        val pods = List(new Pod(0, podsp0), new Pod(1, podsp1)).filter(f => f.size > 0)
        val zone = board.zones.get(zid).get
        zone.occupants = pods
        if (visible == 1) {
          zone.turnLastSeen = currentTurn
          zone.platinumSource = platinum
          zone.owner = ownerid
        }
        if (currentTurn - zone.turnLastSeen > 20) {
          zone.owner = -1
        }

        if (currentTurn == 0 && ownerid != -1) {
          zone.isHeadquarters = true
        }
      }
      val readTime = System.currentTimeMillis() - startTime
      Console.err.println(s"time after reading board: $readTime")

      val maxWait = Duration(90, TimeUnit.MILLISECONDS)
      bfsMoveStrategy.printMoves(maxWait)
      val moveTime = System.currentTimeMillis() - startTime
      Console.err.println(s"Time after calculating moves: $moveTime")

      (new DefaultBuyStrategy).printBuys(board, myid, availablePlatinum)
      currentTurn = currentTurn + 1
    }
  }
}