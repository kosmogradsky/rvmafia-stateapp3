package xyz.razormafia

import scala.util.chaining.scalaUtilChainingOps
import scala.util.Random

case class Playground(
                     players: Set[Playground.Player],
                     state: Playground.State
                     ) {
  def playerConnected(userId: String) = {
    copy(
      players = players.map(
        player =>
          if (player.userId == userId)
            player.connected
          else
            player
      )
    )
  }

  def playerDisconnected(userId: String) = {
    copy(
      players = players.map(
        player =>
          if (player.userId == userId)
            player.disconnected
          else
            player
      )
    )
  }

  def next: Playground = state match {
    case Playground.State.WaitingForPlayers =>
      copy(
        state = Playground.State.Distributing(
          Distribution.create(
            players.map(_.playerNumber).toList.sorted
          )
        )
      )
    case Playground.State.Distributing(distribution) =>
      distribution.pickRole(None) match {
        case PickRoleResult.Continue(nextDistibution) =>
          copy(
            state = Playground.State.Distributing(
              nextDistibution
            )
          )
        case PickRoleResult.Finished(playerRoles) =>
          copy(
            state = Playground.State.Playing(
              Gameplay.create(playerRoles)
            )
          )
      }
    case Playground.State.Playing(gameplay) =>
      copy(
        state = Playground.State.Playing(gameplay.next)
      )
  }

  def nominate(
                nominatorId: String,
                nomineeNumber: Int
              ): Pair[Playground, Boolean] = {
    state match {
      case Playground.State.Playing(gameplay) =>
        val nominatorOption = players.find(_.userId == nominatorId)

        nominatorOption match {
          case Some(nominator) =>
            gameplay
              .nominate(nominator.playerNumber, nomineeNumber)
              .mapFirst(
                nextGameplay =>
                  copy(
                    state = Playground.State.Playing(nextGameplay),
                  )
              )
          case None =>
            Pair(this, false)
        }
      case _ =>
        Pair(this, false)
    }
  }

  def getPlayerId(playerNumber: Int): Option[String] = {
    players
      .find(_.playerNumber == playerNumber)
      .map(_.userId)
  }

  def vote(voterId: String) = {
    state match {
      case Playground.State.Playing(gameplay) =>
        val voterOption = players.find(_.userId == voterId)

        voterOption match {
          case Some(voter) =>
            copy(
              state = Playground.State.Playing(
                gameplay.vote(
                  voter.playerNumber
                )
              )
            )
          case None =>
            this
        }
      case _ =>
        this
    }
  }
}

object Playground {
  enum State {
    case WaitingForPlayers
    case Distributing(distribution: Distribution)
    case Playing(gameplay: Gameplay)
  }

  case class Player(
                   playerNumber: Int,
                   userId: String,
                   isConnected: Boolean
                   ) {
    def connected = {
      copy(
        isConnected = true
      )
    }

    def disconnected = {
      copy(
        isConnected = false
      )
    }
  }

  def fromPlayerIds(playerIds: List[String]): Playground = {
    val playerEntries = playerIds
      .zipWithIndex
      .map(
        (uid, index) => Player(index + 1, uid, false)
      )

    Playground(Set.from(playerEntries), State.WaitingForPlayers)
  }
}