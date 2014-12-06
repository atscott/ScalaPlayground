import scala.util.Try

val temps = "-20 5 4 30 -2 9"

val tempInts = Try(temps.split(" ").map(_.toInt))
if (tempInts.isFailure) println(0)
else {
  val closestNegative = tempInts.get.filter(_ < 0).sortWith(_ > _).headOption
  val closestPositive = tempInts.get.filter(_ > 0).sorted.headOption

  val res = (closestNegative, closestPositive) match {
    case (Some(n), Some(p)) =>
      if (Math.abs(n) == Math.abs(p)) p
      else if (Math.abs(n) < Math.abs(p)) n
      else p
    case (None, Some(p)) => p
    case (Some(n), None) => n
    case _ => 0
  }

  println(res)
}
