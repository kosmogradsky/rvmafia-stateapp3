package xyz.razormafia

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSExportTopLevel("StateApp")
object StateApp {
  var state = State.initial

  @JSExport
  var onQueueSizeChanged: js.Function1[Int, Unit] =
    (queueSize: Int) => ()

  @JSExport
  var onMatchCreated: js.Function1[String, Unit] =
    (gameId: String) => ()

  @JSExport
  var onNominated: js.Function1[NominationEvent, Unit] =
    (data: NominationEvent) => ()

  def updateAndNotifyListeners(nextState: State): Unit = {
    val prevState = state
    state = nextState

    println(state)
    if (prevState.queue.size != state.queue.size) {
      onQueueSizeChanged(state.queue.size)
    }
  }

  @JSExport
  def addQueueEntry(uid: String): Unit = {
    updateAndNotifyListeners(state.addQueueEntry(uid))
  }

  @JSExport
  def removeQueueEntry(uid: String): Unit = {
    updateAndNotifyListeners(state.removeQueueEntry(uid))
  }

  @JSExport
  def createMatchIfEnoughPlayers(gameId: String): Unit = {
    val (wasMatchCreated, nextState) = state.createMatchIfEnoughPlayers(gameId)

    if (wasMatchCreated) {
      onMatchCreated(gameId)
    }

    updateAndNotifyListeners(nextState)
  }

  @JSExport
  def next(gameId: String): Unit = {
    updateAndNotifyListeners(state.next(gameId))
  }

  @JSExport
  def nominate(
              playgroundId: String,
              nominatorId: String,
              nomineeNumber: Int
              ) = {
    val pair = state.nominate(
      playgroundId,
      nominatorId,
      nomineeNumber
    )

    if (pair.second) {
      val nomineeId = state.getPlayerId(playgroundId, nomineeNumber)

      if (nomineeId.nonEmpty) {
        onNominated(
          NominationEvent(
            playgroundId,
            nomineeId.get
          )
        )
      }
    }

    updateAndNotifyListeners(pair.first)
  }

  @JSExport
  def vote(
          playgroundId: String,
          voterId: String
          ) = {
    val nextState = state.vote(
      playgroundId,
      voterId
    )

    updateAndNotifyListeners(nextState)
  }

  @JSExport
  def playerConnected(
                     playgroundId: String,
                     userId: String
                     ) = {
    val nextState = state.playerConnected(
      playgroundId,
      userId
    )

    updateAndNotifyListeners(nextState)
  }

  @JSExport
  def playerDisconnected(
                        playgroundId: String,
                        userId: String
                        ) = {
    val nextState = state.playerDisconnected(
      playgroundId,
      userId
    )

    updateAndNotifyListeners(nextState)
  }

  @JSExport
  def getPlayerId(
                 playgroundId: String,
                 playerNumber: Int
                 ): String = {
    state.getPlayerId(playgroundId, playerNumber).getOrElse(null)
  }
}
