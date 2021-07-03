package xyz.razormafia

enum PickRoleResult {
  case Continue(distribution: Distribution)
  case Finished(playersWithRole: List[(Int, PlayerRole)])
}
