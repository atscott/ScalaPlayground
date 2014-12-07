val rows = List(
  " #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ### ",
  "# # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # ",
  "### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## ",
  "# # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       ",
  "# # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  ")

val wantedLetters = "WMADXESFGIJVTHKNOBPLYQRUZC"

/*
Option 1
 */
val allLetters = wantedLetters.map(m => getLetter(m, rows, 4)).toList

for (index <- 0 until allLetters.head.length) {
  printIndex(index, allLetters)
}

def getLetter(c: Char, letters: List[String], l: Int): List[String] = {
  val letterNum = c.toUpper - 'A'
  if (letterNum < 0 || letterNum > 25)
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


/*
Option 2
 */

printLetters(wantedLetters, rows, 4)

def printLetters(letters: String, ascii: List[String], length: Int) = {
  for (h <- 0 until ascii.length) {
    for (c <- letters) {
      val letterNum = c.toUpper - 'A'
      if (letterNum < 0 || letterNum > 25)
        print(ascii.drop(h).head.substring(26 * length, 26 * length + length))
      else {
        val start = letterNum * length
        print(ascii.drop(h).head.substring(start, start + length))
      }
    }
    println("")
  }
}
