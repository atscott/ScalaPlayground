val count = 2 // The number of current enemy ships within range
val enemies = for(i <- 0 until count) yield {
    // enemy: The name of this enemy
    // dist: The distance to your cannon of this enemy
    val Array(enemy, _dist) = "enemy 1" split " "
    val dist = _dist.toInt
    (enemy, dist)
  }

println(enemies.minBy(_._2)._1)