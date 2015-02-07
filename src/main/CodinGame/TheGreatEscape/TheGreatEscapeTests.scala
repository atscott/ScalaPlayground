package CodinGame.TheGreatEscape

import CodinGame.TheGreatEscape.Player._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TheGreatEscapeTests extends FunSuite {
  test("cannot travel to position blocked by horizontal wall going down with wall below 1") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(1,2,'H')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(1, 2)))
  }
  test("cannot travel to position blocked by horizontal wall going down with wall below") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(0,2,'H')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(1, 2)))
  }
  test("cannot travel to position blocked by vertical wall case 1") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(2,1,'V')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(2, 1)))
  }
  test("cannot travel to position blocked by vertical wall case 2") {
    val board = Board(List(PlayerClass(Position(1, 1), 0, 0)), 4, 4, List(Wall(2,0,'V')))
    val determiner = new PathDeterminer(board)
    assert(!determiner.canTravel(Position(1, 1), Position(2, 1)))
  }
  test("can travel to position not blocked by vertical wall going right with wall above"){
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2,0,'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(1, 2), Position(2, 2)))
  }
  test("can travel to position not blocked by vertical wall going right with wall below"){
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2,2,'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(1, 1), Position(2, 1)))
  }
  test("can travel to position not blocked by vertical wall going left with wall above"){
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2,0,'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(2, 2), Position(1, 2)))
  }

  test("can travel to position not blocked by vertical wall going left with wall below"){
    val board = Board(List(PlayerClass(Position(1, 2), 0, 0)), 4, 4, List(Wall(2,2,'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.canTravel(Position(2, 1), Position(1, 1)))
  }

  test("distance no obstacles, going right"){
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List())
    val determiner = new PathDeterminer(board)
    assert(determiner.shortestPathToFinish(0).size == 3)
  }

  test("distance one obstacle in the way twice, going right"){
    val board = Board(List(PlayerClass(Position(0, 0), 0, 0)), 4, 4, List(Wall(1, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.shortestPathToFinish(0).size == 5)
  }

  test("distance one obstacle in the way once, going right"){
    val board = Board(List(PlayerClass(Position(0, 1), 0, 0)), 4, 4, List(Wall(1, 0, 'V')))
    val determiner = new PathDeterminer(board)
    assert(determiner.shortestPathToFinish(0).size == 4)
  }

}
