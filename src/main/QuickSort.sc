def QuickSort(l: List[Int]): List[Int] =
  l match {
    case (x :: xs) =>
      val sortedLeft = QuickSort(xs.filter(f => f <= x))
      val sortedRight = QuickSort(xs.filter(f => f > x))
      sortedLeft ++ List(x) ++ sortedRight
    case _ => List()
  }

QuickSort(List(1, 4, 3, 9, 7, 6, 8))
