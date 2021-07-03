package xyz.razormafia

import scala.math.max

case class State(
                queue: Map[String, QueueEntry],
                playgrounds: Map[String, Playground],
                ) {
  def addQueueEntry(uid: String): State = {
    copy(
      queue = queue + (uid -> QueueEntry.create())
    )
  }

  def removeQueueEntry(uid: String): State = {
    copy(
      queue = queue - uid
    )
  }

  def createMatchIfEnoughPlayers(gameId: String): (Boolean, State) = {
    val (participants, waiters) = queue
      .toList
      .sortBy(_._2.orderToken)
      .splitAt(10)

    if (participants.length == 10) {
      val playerIds = participants.map(_._1)
      val nextState = copy(
        queue = Map.from(waiters),
        playgrounds = playgrounds +
          (gameId -> Playground.fromPlayerIds(playerIds))
      )

      (true, nextState)
    } else {
      val promotedWaiters = participants.map(waiter =>
        (
          waiter._1,
          QueueEntry.create(
            max(waiter._2.orderTokenRange / 2, 1)
          )
        )
      )
      val nextState = copy(
        queue = Map.from(promotedWaiters)
      )

      (false, nextState)
    }
  }

  def next(playgroundId: String): State = {
    copy(
      playgrounds = playgrounds
        .updatedWith(playgroundId) {
          _.map(_.next)
        }
    )
  }

  def nominate(
                playgroundId: String,
                nominatorId: String,
                nomineeNumber: Int
              ): Pair[State, Boolean] = {
    playgrounds.get(playgroundId) match {
      case Some(playground) =>
        playground
          .nominate(nominatorId, nomineeNumber)
          .mapFirst(
            nextPlayground =>
              copy(
                playgrounds = playgrounds.updated(
                  playgroundId,
                  nextPlayground
                )
              )
          )
      case None =>
        Pair(this, false)
    }
  }

  def vote(
          playgroundId: String,
          voterId: String
          ) = {
    playgrounds.get(playgroundId) match {
      case Some(playground) =>
        copy(
          playgrounds = playgrounds.updated(
            playgroundId,
            playground.vote(
              voterId
            )
          )
        )
      case None =>
        this
    }
  }

  def getPlayerId(playgroundId: String, playerNumber: Int): Option[String] = {
    playgrounds
      .get(playgroundId)
      .flatMap(_.getPlayerId(playerNumber))
  }

  def playerConnected(gameId: String, userId: String) = {
    copy(
      playgrounds = playgrounds.updatedWith(gameId) {
        _.map(_.playerConnected(userId))
      }
    )
  }

  def playerDisconnected(gameId: String, userId: String) = {
    copy(
      playgrounds = playgrounds.updatedWith(gameId) {
        _.map(_.playerDisconnected(userId))
      }
    )
  }
}

object State {
  val initial = State(Map.empty, Map.empty)
}
