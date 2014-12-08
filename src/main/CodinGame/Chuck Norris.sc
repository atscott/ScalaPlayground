Chuckify("C")
Chuckify("CC")

def Chuckify(s: String): String = {
  val bits = s map charTo7BitString
  val combined = bits.foldLeft("")(_ + _)

  def iter(s: String, i: Int): String =
    if (i >= combined.length) s
    else if (combined.charAt(i - 1) == combined.charAt(i)) iter(s + "0", i + 1)
    else if (combined.charAt(i) == '1') iter(s + " 0 0", i + 1)
    else iter(s + " 00 0", i + 1)


  if (combined.length < 1) ""
  else if (combined.startsWith("1")) iter("0 0", 1)
  else iter("00 0", 1)
}

def charTo7BitString(c: Char): String = {
  def iter(i: Int, s: String): String =
    if (i > 6) s
    else if ((c & (1 << i)) == (1 << i)) iter(i + 1, "1" + s)
    else iter(i + 1, "0" + s)

  iter(0, "")
}