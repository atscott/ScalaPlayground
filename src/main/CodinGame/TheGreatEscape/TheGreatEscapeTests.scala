package CodinGame.TheGreatEscape

import CodinGame.TheGreatEscape.Player._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TheGreatEscapeTests extends FunSuite {
  test("cannot travel to position blocked by horizontal wall going down with wall below 1") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(1, 2, 'H')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(1, 2)))
  }
  test("cannot travel to position blocked by horizontal wall going down with wall below") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(0, 2, 'H')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(1, 2)))
  }
  test("cannot travel to position blocked by vertical wall case 1") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(2, 1, 'V')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(2, 1)))
  }
  test("cannot travel to position blocked by vertical wall case 2") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(2, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(2, 1)))
  }
  test("can travel to position not blocked by vertical wall going right with wall above") {
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(1, 2), Position(2, 2)))
  }
  test("can travel to position not blocked by vertical wall going right with wall below") {
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2, 2, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(1, 1), Position(2, 1)))
  }
  test("can travel to position not blocked by vertical wall going left with wall above") {
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(2, 2), Position(1, 2)))
  }

  test("can travel to position not blocked by vertical wall going left with wall below") {
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2, 2, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(2, 1), Position(1, 1)))
  }

  test("distance no obstacles, going right") {
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List())
    val determiner = new PathDeterminer(board)
    assert(determiner.shortestPathToFinish(0).size == 3)
  }

  test("distance one obstacle in the way twice, going right") {
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List(Wall(1, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.shortestPathToFinish(0).size == 5)
  }

  test("distance one obstacle in the way once, going right") {
    val board = Board(List(PlayerClass(Position(0, 1), 0, 0)), 4, 4, List(Wall(1, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.shortestPathToFinish(0).size == 4)
  }

  test("intersection") {
    /*
    *-* *

    * *-*

    * * *
     */
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val intersect = wallDeterminer.doIntersect(Position(0, 0), Position(0, 1), Position(1, 1), Position(1, 2))
    assert(!intersect)
  }

  test("intersection 2") {
    /*
    * * *
    |
    *-*-*
    |
    * * *
     */
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val intersect = wallDeterminer.fullyIntersect(Position(0, 0), Position(0, 2), Position(0, 1), Position(2, 1))
    assert(!intersect)
  }

  test("intersection 3") {
    /*
    * * *
      |
    *-*-*
      |
    * * *
     */
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val intersect = wallDeterminer.doIntersect(Position(0, 1), Position(2, 1), Position(1, 0), Position(1, 2))
    assert(intersect)
  }

  test("intersection 4") {
    /*
    * * *
      |
    * * *
      |
    * * *
     */
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val intersect = wallDeterminer.fullyIntersect(Position(0, 1), Position(1, 1), Position(1, 1), Position(2, 1))
    assert(!intersect)
  }


  test("intersection 5") {
    /*
      * * *

      *_*_*
       - -
      * * *
     */
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val intersect = wallDeterminer.fullyIntersect(Position(1, 0), Position(1, 2), Position(1, 0), Position(1, 2))
    assert(intersect)
  }

  test("intersection 6") {
    /*
    * * *
      ||
    * * *

    * * *
     */
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val intersect = wallDeterminer.fullyIntersect(Position(0, 1), Position(1, 1), Position(0, 1), Position(1, 1))
    assert(intersect)
  }

  test("intersection 7") {
    val wallDeterminer = new WallDeterminer(Board(List(), 0, 0, List()))
    val wall1 = Wall(7, 7, 'H')
    val wall2 = Wall(5, 7, 'H')
    assert(!wallDeterminer.fullyIntersect(wall2.p1, wall2.p2, wall1.p1, wall1.p2))
    assert(!wallDeterminer.fullyIntersect(wall1.p1, wall1.p2, wall2.p1, wall2.p2))
  }


  test("within bounds vertical") {
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List())
    val wallDeterminer = new WallDeterminer(board)
    assert(!wallDeterminer.wallIsWithinBounds(Wall(3, 3, 'V'), board))
  }

  test("within bounds horizontal") {
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List())
    val wallDeterminer = new WallDeterminer(board)
    assert(!wallDeterminer.wallIsWithinBounds(Wall(3, 3, 'H'), board))
  }

  test("getWallToBlock 1 ") {
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List())
    val wallDeterminer = new WallDeterminer(board)
    val paths = List(Position(1, 0), Position(2, 0), Position(3, 0))
    val wall = wallDeterminer.getWalltoBlock(board.players.head, paths)
    assert(wallDeterminer.wallIsWithinBounds(wall.get, board))
    assert(wall.get == Wall(1, 0, 'V'))
  }

  test("getWallToBlock 2") {
    //starting at bottom right corner, player with id 1 goes left
    val board = Board(List(PlayerClass(Position(3, 3), 0, 1)), 4, 4, List())
    val wallDeterminer = new WallDeterminer(board)
    val paths = List(Position(2, 3), Position(1, 3), Position(0, 3))
    val wall = wallDeterminer.getWalltoBlock(board.players.head, paths)
    assert(wall.isDefined)
    assert(wallDeterminer.wallIsWithinBounds(wall.get, board))
  }


}
