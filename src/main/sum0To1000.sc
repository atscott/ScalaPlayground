(0 to 999).filter(x => x % 3 == 0 || x % 5 == 0).fold(0)(_+_)

(0 to 999).filter(x => x % 3 == 0 || x % 5 == 0).sum

(0 to 999 by 3).sum + (0 to 999 by 5).sum - (0 to 999 by 15).sum

((0 to 999 by 3) ++ (0 to 999 by 5)).sum - (0 to 999 by 15).sum

((0 to 999 by 3) ++ (0 to 999 by 5)).toSet[Int].sum
