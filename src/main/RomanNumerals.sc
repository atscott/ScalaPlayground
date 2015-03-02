def Roman(n: Int): String = 
  scala.collection.SortedMap[Int, String](
    100 -> "C",
    90 -> "XC", 
    50 -> "L",
    40 -> "XL",
    10 -> "X",
    9 -> "IX",
    5 -> "V", 
    4 -> "IV",
    1 -> "I")
  .foldRight("I" * n)((kv, acc) => acc.replace("I" * kv._1, kv._2))

Roman(5)
Roman(4)
Roman(99)
Roman(101)
