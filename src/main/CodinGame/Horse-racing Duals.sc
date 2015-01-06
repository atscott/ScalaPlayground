val strengths = List(10, 5, 15, 17, 3, 8, 11, 28, 6, 55, 7)

val sorted = strengths.sorted
sorted
  .zip(sorted.tail)
  .foldRight(Int.MaxValue)({ case ((i, j), min) => math.min(min, math.abs(i - j))})