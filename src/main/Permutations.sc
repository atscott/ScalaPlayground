//Get other permutations, insert head into every index of each permutation
def permutations1(s: String): List[String] =
  if (s.length < 2) List(s)
  else
    for {
      permutation <- permutations1(s.drop(1))
      index <- 0 to permutation.length
    } yield permutation.substring(0, index) + s.head.toString + permutation.substring(index)

//For every letter in string, get permutations of other letters and prepend those with the removed letter
def permutations2(s: String): List[String] =
  (for (c <- s) yield {
    val rest = s.replaceFirst(c.toString, "")
    if (rest.length > 0) permutations2(rest).map(s => c + s)
    else List(c.toString)
  }).flatten.toList

permutations2("a")
permutations1("a")
permutations2("ab")
permutations1("ab")
permutations2("abc")
permutations1("abc")
permutations2("abcd")
permutations1("abcd")
