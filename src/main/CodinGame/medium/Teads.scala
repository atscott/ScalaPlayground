import scala.collection.mutable

object Solution {

  case class CacheKey(start: Int, parent: Option[Int])

  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    var adjacencyList = mutable.Map[Int, List[Int]]()
    for (i <- 0 until n) {
      val Array(xi, yi) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
      adjacencyList += xi -> (yi :: adjacencyList.getOrElse(xi, List()))
      adjacencyList += yi -> (xi :: adjacencyList.getOrElse(yi, List()))
    }

    var depthCache = mutable.Map[CacheKey, Int]()
    def depth(start: Int, parent: Option[Int]): Int = {
      val cacheKey = CacheKey(start, parent)
      if (depthCache.contains(cacheKey)) {
        depthCache(cacheKey)
      } else {
        val neighbors = adjacencyList(start) filter { f => Some(f) != parent}
        val myDepth =
          if (neighbors.isEmpty) 0
          else 1 + (neighbors map { n => depth(n, Some(start))}).max
        depthCache += cacheKey -> myDepth
        myDepth
      }
    }

    val result = adjacencyList.keys.foldRight(n)((key, min) => math.min(depth(key, None), min))

    println(result)
  }
}