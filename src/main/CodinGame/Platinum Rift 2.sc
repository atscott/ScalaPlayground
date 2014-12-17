import scala.collection.immutable.HashMap

class Pod(val owner: Int, var size: Int) {
}

class Zone(val id: Int, val platinumSource: Int, var owner: Int, var occupants: List[Pod]) {
  override def toString: String = "zone " + id + "; platinum " + platinumSource + "; owner " + owner
}

class BuyCommand(val zone: Zone, val numToBuy: Int) {}

class Move(val origin: Int, val destination: Int) {
  override def toString: String = "zone " + origin + " -> zone " + destination
}


class Board(val zones: HashMap[Int, Zone], val adjacencyList: Map[Int, Set[Int]]) {
  def neighborsWithHistory(z: Zone, history: List[Move]): Stream[(Zone, List[Move])] = {
    val adjacentZones = adjacencyList.get(z.id)
    if (adjacentZones.isEmpty) Stream()
    else
      adjacentZones
        .head
        .map(zoneId => (zones.get(zoneId).head, new Move(z.id, zoneId) :: history))
        .toStream
  }

  def getPlayerPodSizeForZone(zoneId: Int, playerId: Int): Int = {
    val pod = zones.get(zoneId).get.occupants.filter(p => p.owner == playerId)
    pod.headOption match {
      case Some(x) => x.size
      case _ => 0
    }
  }


  def newNeighborsOnly(neighbors: Stream[(Zone, List[Move])], explored: Set[Zone]): Stream[(Zone, List[Move])] =
    neighbors.filter { case (block, ml) => !explored.contains(block)}

  def from(initial: Stream[(Zone, List[Move])], explored: Set[Zone]): Stream[(Zone, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    val possibleMoves = for {
      (zone, moveList) <- initial
      possibleMove <- newNeighborsOnly(neighborsWithHistory(zone, moveList), explored)
    } yield possibleMove
    val updatedExplored = explored ++ possibleMoves.map(f => f._1)
    if (possibleMoves.isEmpty) initial
    else initial #::: from(possibleMoves.toStream, updatedExplored)
  }
}

val zones = List(new Zone(1, 1, -1, List(new Pod(0, 1))), new Zone(2, 0, -1, List()), new Zone(3, 4, -1, List()))
val zm = HashMap(zones.map(m => (m.id, m)): _*)
val adjacencies = HashMap((1, Set(2)), (2, Set(1)), (2, Set(3)), (3, Set(2)))
val board = new Board(zm, adjacencies)
printBuys(board, 0, 1)

printMoves(board, 0)

def printMoves(b: Board, player: Int) = {
  val zonesWithPlayerPods = b.zones.filter(f => f._2.occupants.exists(pod => pod.owner == player))
  val moves = zonesWithPlayerPods
    .map(f => getTargetMoveZone(b, f._2, player))
    .filter(_.size > 0)
    .map(m => m.head)
  val moveString = moves.foldLeft("")((r, c) => r + 1 + " " + c._2.last.origin + " " + c._2.last.destination + " ")
  println(moveString)
}

def getTargetMoveZone(board: Board, startZone: Zone, player: Int): Stream[(Zone, List[Move])] = {
  val moves = board.from(Stream((startZone, List())), Set())
  val movesWithin15 = moves.filter(f => f._2.size < 15)
  val sorted = movesWithin15.sortWith((p1, p2) => p1._2.size - p1._1.platinumSource < p2._2.size - p2._1.platinumSource)
  sorted.filter(f => f._1.owner != player)
}


def printBuys(board: Board, player: Int, availablePlatinum: Int) = {
  val buys = getBuyTargets(board, player, availablePlatinum)
  val buyString = buys.foldLeft("")((r, c) => r + c.numToBuy + " " + c.zone.id + " ")
  println(buyString)
}

def getBuyTargets(board: Board, player: Int, availablePlatinum: Int): List[BuyCommand] = {
  val zonesWithPlayerPods = board.zones.filter(f => f._2.occupants.exists(pod => pod.owner == player)).toList
  val sorted = zonesWithPlayerPods.sortWith((s1, s2) => board.getPlayerPodSizeForZone(s1._2.id, player) < board.getPlayerPodSizeForZone(s2._2.id, player))
  List(new BuyCommand(sorted.head._2, availablePlatinum))
}

