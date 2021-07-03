package xyz.razormafia

import xyz.razormafia.Distribution.shuffleRoles

import scala.util.chaining.scalaUtilChainingOps
import scala.util.Random

case class Distribution(
                       keyOfPlayerWhoPicks: Int,
                       playersWithRole: List[(Int, PlayerRole)],
                       keysOfPlayersWithoutRole: List[Int],
                       rolesPool: Map[Int, PlayerRole]
                       ) {
  def pickRole(roleKeyOption: Option[Int]): PickRoleResult = {
    val pickedRoleWithIndex = for {
      roleKey <- roleKeyOption
      role <- rolesPool.get(roleKey)
    } yield (roleKey, role)

    val (roleKey, role) = pickedRoleWithIndex getOrElse {
      val rolesVector = rolesPool.toVector

      Random
        .between(0, rolesVector.length)
        .pipe(rolesVector.apply)
    }

    val playerWithRole = (
      keyOfPlayerWhoPicks,
      role
    )

    keysOfPlayersWithoutRole match {
      case keyOfPlayerWithoutRole::remainingKeys =>
        PickRoleResult.Continue(
          Distribution(
            keyOfPlayerWithoutRole,
            playerWithRole::playersWithRole,
            remainingKeys,
            shuffleRoles(rolesPool - roleKey)
          )
        )
      case Nil =>
        PickRoleResult.Finished(
          (playerWithRole::playersWithRole).reverse
        )
    }
  }
}

object Distribution {
  def create(players: List[Int]) = {
    val playerWhoPicks::playersWithoutRole = players

    Distribution(
      playerWhoPicks,
      List.empty,
      playersWithoutRole,
      shuffleRoles(tenRoles)
    )
  }

  val tenRoles: Map[Int, PlayerRole] = Map(
    1 -> PlayerRole.Civilian,
    2 -> PlayerRole.Civilian,
    3 -> PlayerRole.Civilian,
    4 -> PlayerRole.Civilian,
    5 -> PlayerRole.Civilian,
    6 -> PlayerRole.Civilian,
    7 -> PlayerRole.Mafia,
    8 -> PlayerRole.Mafia,
    9 -> PlayerRole.Godfather,
    10 -> PlayerRole.Sheriff
  )

  def shuffleRoles(roles: Map[Int, PlayerRole]): Map[Int, PlayerRole] = {
    val (roleKeys, roleValues) = roles.unzip

    roleKeys zip Random.shuffle(roleValues) pipe Map.from
  }
}