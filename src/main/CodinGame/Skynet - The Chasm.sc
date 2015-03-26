
def getAction(road: Int, gap: Int, speed: Int, position: Int): String = {
      if (position > road) "SLOW"
      else if (road - (position + speed) < 0) "JUMP"
      else if (speed == gap+1) "WAIT"
      else if (speed < gap+1) "SPEED"
      else "SLOW"
}
