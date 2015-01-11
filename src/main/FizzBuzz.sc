val ints: Stream[Int] = 1 #:: 2 #:: ints.zip(ints.tail).map(n => n._2 + 1)

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

fizzBuzz.drop(2).head
fizzBuzz.take(15).toList
fizzBuzz.drop(2999).head
