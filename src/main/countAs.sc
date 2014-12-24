def countLowercaseAs(s: String): BigInt = {
  if (s == null) 0
  else s.foldLeft(0)((acc, c) => if (c == 'a') acc + 1 else acc)
}

def countChars(p: Char => Boolean)(s: String): BigInt = {
  if (s == null) 0
  else s.foldLeft(0)((acc, char) => if (p(char)) acc + 1 else acc)
}

def countLowercaseAsCurried(s: String): BigInt = countChars(c => c == 'a')(s)

countLowercaseAs("assdfsaasdfaAA")
countLowercaseAsCurried("assdfsaasdfaAAA")
countChars(c => c >= 'a' && c <= 'g')("assdfsaasdfa")
countChars(c => c == 'a' || c == 'A')("assdfsaasdfaAAA")
countLowercaseAs(null)
countLowercaseAs("")
countLowercaseAs("b")
