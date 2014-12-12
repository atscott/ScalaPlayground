val defibStrings = List("1;Maison de la Prevention Sante;6 rue Maguelone 340000 Montpellier;;3,87952263361082;43,6071285339217",
  "2;Hotel de Ville;1 place Georges Freche 34267 Montpellier;;3,89652239197876;43,5987299452849",
  "3;Zoo de Lunaret;50 avenue Agropolis 34090 Mtp;;3,87388031141133;43,6395872778854")

val defibrillators = for (d <- defibStrings) yield {
  (d split ";").toList
}

findClosestDefibrillator(defibrillators, 3.8, 43)

def findClosestDefibrillator(defibrillators: List[List[String]], myLat: Double, myLong: Double): List[String] =
  defibrillators.minBy(m => distance(getLatitude(m), getLongitude(m), myLat, myLong))

def distance(lat1: Double, long1: Double, lat2: Double, long2: Double): Double = {
  val x = (long2 - long1) * math.cos((lat1 + lat2) / 2)
  val y = lat2 - lat1
  math.sqrt(x * x + y * x) * 6371
}

def getLatitude(d: List[String]): Double = getCoordinate(4)(d)
def getLongitude(d: List[String]): Double = getCoordinate(5)(d)
def getCoordinate(i: Int)(d: List[String]): Double =
  defibrillators.head.drop(i).head.replace(',', '.').toDouble
