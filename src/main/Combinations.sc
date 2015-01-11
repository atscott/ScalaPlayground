def combintations(s: String) =
  (for (mask <- 1 until 1 << s.length) yield {
    (for {
      i <- 0 until s.length
      if (1 << i & mask) == (1 << i)
    } yield s.charAt(i).toString).mkString("")
  }).toList

combintations("ab")
combintations("abc")