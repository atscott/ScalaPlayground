
def find(list: Array[Int], target: Int): Int = {
  def findIter(start: Int, end: Int): Int = {
    def mid = start + (end - start) / 2
    if (list(mid) == target) mid
    else if (start == end) -1
    else if (list(mid) < target) findIter(mid + 1, end)
    else findIter(start, mid - 1)
  }

  if (list.isEmpty || list == null) return -1
  findIter(0, list.length - 1)
}

find(Array(1, 2, 34, 142, 300), 1)


def abc = Array(1, 2, 3, 4)
abc(1)
