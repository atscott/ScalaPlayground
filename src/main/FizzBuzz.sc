val ints: Stream[Int] = 0 #:: 1 #:: ints.zip(ints.tail).map(n => n._2 + 1)

val fizzes: Stream[String] = ints.map(i => if (i % 3 == 0) "Fizz" else "")
val buzzes: Stream[String] = ints.map(i => if (i % 5 == 0) "Buzz" else "")

val fizzesAndBuzzes =
  fizzes
    .zip(buzzes)
    .map { case (fizz, buzz) => fizz + buzz}
val fizzBuzz =
  fizzesAndBuzzes
    .zip(ints)
    .map { case (fizzbuzz, i) => if (fizzbuzz == "") i.toString else fizzbuzz}

fizzBuzz(3)
fizzBuzz.take(15).toList
fizzBuzz(3000)
