/*
 http://community.topcoder.com/stat?c=problem_statement&pm=3561&rd=6519
*/
def howMany(c: Int, time: Int): Double = {
  val target = time.toDouble / c

  def guess(lower: Double, upper: Double, previousGuess: Double): Double = {
    val currentGuess = (upper + lower) / 2
    val result = currentGuess * math.log(currentGuess) / math.log(2)

    if (math.abs(result - target) < "1E-9".toFloat || currentGuess == previousGuess)
      currentGuess
    else if (result > target)
      guess(lower, currentGuess, currentGuess)
    else
      guess(currentGuess, upper, currentGuess)
  }

  guess(0, target, -1)
}

howMany(1, 8)
howMany(2, 16)
howMany(37, 12392342)
howMany(1, 2000000000)