
def getAction(road: Int, gap: Int, speed: Int, position: Int): String = {
  if (position > road) "SLOW"
  else if (road - (position + speed) < 0) "JUMP"
  else if (speed > gap) "WAIT"
  else "SPEED"
}