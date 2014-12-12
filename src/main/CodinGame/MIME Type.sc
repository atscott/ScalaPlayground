import scala.collection.immutable.HashMap

val files = List("test.html", "noextension", "portrait.png", "doc.TXT")
val temp = List(("html", "text/html"), ("png", "image/png")).map(m => (m._1.toUpperCase, m._2))
val types = HashMap(temp: _*)

def getMimeType(file: String, types: HashMap[String, String]): String = {
  val lastPeriodIndex = file.lastIndexOf('.')
  if (lastPeriodIndex < 0) "UNKNOWN"
  else
    types.get(file.substring(lastPeriodIndex + 1).toUpperCase) match {
      case Some(x) => x
      case _ => "UNKNOWN"
    }
}

for (f <- files) {
  println(getMimeType(f, types))
}