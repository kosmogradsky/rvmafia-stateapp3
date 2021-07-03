package xyz.razormafia

enum Fouls(val asInt: Int) {
  case Zero extends Fouls(0)
  case One extends Fouls(1)
  case Two extends Fouls(2)
  case Three extends Fouls(3)
}
