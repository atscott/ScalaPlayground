def getMoves(moves: List[String], currentState: List[Int]): List[String] = {
  currentState match {
    case lx :: ly :: tx :: ty :: rest =>
      if (lx == tx && ly == ty) moves
      else {
        val yMove = NorS(ly, ty)
        val xMove = EorW(lx, tx)
        getMoves((yMove._1 + xMove._1) :: moves, List(lx, ly, tx + xMove._2, ty + yMove._2))
      }
    case _ => moves
  }
}

def NorS(ly: Int, ty: Int): (String, Int) =
  if (ly > ty) ("S", 1)
  else if (ly < ty) ("N", -1)
  else ("", 0)

def EorW(lx: Int, tx: Int): (String, Int) =
  if (lx > tx) ("E", 1)
  else if (lx < tx) ("W", -1)
  else ("", 0)

val test1 = Array(2, 3, 10, 15).toList
val test2 = Array(31, 4, 31, 17).toList

getMoves(List(), test1)
getMoves(List(), test2)

