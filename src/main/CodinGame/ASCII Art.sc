val rows = List(
  " #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ### ",
  "# # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # ",
  "### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## ",
  "# # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       ",
  "# # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  ")

val wantedLetters = "@"

val allLetters = wantedLetters.map(m => getLetter(m, rows, 4)).toList

for (index <- 0 until allLetters.head.length) {
  printIndex(index, allLetters)
}

def getLetter(c: Char, letters: List[String], l: Int): List[String] = {
  val letterNum = c.toUpper - 'A'
  if (letterNum < 0 || letterNum > 24)
    letters.map(m => m.substring(26 * l, 26 * l + l))
  else {
    val start = letterNum * l
    letters.map(m => m.substring(start, start + l))
  }
}

def printIndex(n: Int, l: List[List[String]]) = {
  l.foreach(f => print(f.drop(n).head))
  println("")
}
