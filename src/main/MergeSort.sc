def MergeSort(l: List[Int]): List[Int] = {
  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (xs1, Nil) => xs
      case (Nil, ys1) => ys
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

  val n = l.length / 2
  if (n == 0) l
  else {
    val left = MergeSort(l.take(l.size / 2))
    val right = MergeSort(l.drop(l.size / 2))
    merge(left, right)
  }
}

//Same thing, but merge is tail recursive
def MergeSort2(l: List[Int]): List[Int] = {
  def merge(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] =
    (xs, ys) match {
      case (xs1, Nil) => xs
      case (Nil, ys1) => ys
      case (x :: xs1, y :: ys1) =>
        if (x < y) merge(xs1, ys, x :: acc)
        else merge(xs, ys1, y :: acc)
    }

  val n = l.length / 2
  if (n == 0) l
  else {
    val left = MergeSort2(l.take(l.size / 2))
    val right = MergeSort2(l.drop(l.size / 2))
    merge(left, right, List())
  }
}
MergeSort(List(1, 3, 2, 6, 8, 4))
