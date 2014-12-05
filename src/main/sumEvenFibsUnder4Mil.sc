def isEven(x: BigInt): Boolean = x % 2 == BigInt(0)

def fib(): Stream[BigInt] = {
  def iter(prev1: BigInt, prev2: BigInt): Stream[BigInt] =
    (prev1 + prev2) #:: iter(prev2, prev1 + prev2)

  iter(1, 1)
}

fib().takeWhile(_ < 4000000).filter(isEven).sum

val fibs: Stream[BigInt] =
  BigInt(1) #:: BigInt(2) #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)

fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum