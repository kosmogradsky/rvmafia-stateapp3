package xyz.razormafia

import utest._

object StateTest extends TestSuite {
  val tests = Tests {
    test("createMatchIfEnoughPlayers") {
      test("initialState") {
        State.initial.createMatchIfEnoughPlayers("gameId")
      }

      test("withLessThanTenPlayers") {
        val state = State.initial
          .addQueueEntry("playerOne")
          .addQueueEntry("playerTwo")
          .addQueueEntry("playerThree")
          .addQueueEntry("playerFour")
          .addQueueEntry("playerFive")
          .addQueueEntry("playerSix")
          .createMatchIfEnoughPlayers("gameId")

        assert(state.queue.nonEmpty)
        assert(state.queue.forall {
          case (playerId, queueEntry) =>
            queueEntry.orderTokenRange == 512
        })

        state
      }

      test("withExactlyTenPlayers") {
        val state = State.initial
          .addQueueEntry("playerOne")
          .addQueueEntry("playerTwo")
          .addQueueEntry("playerThree")
          .addQueueEntry("playerFour")
          .addQueueEntry("playerFive")
          .addQueueEntry("playerSix")
          .addQueueEntry("playerSeven")
          .addQueueEntry("playerEight")
          .addQueueEntry("playerNine")
          .addQueueEntry("playerTen")
          .createMatchIfEnoughPlayers("gameId")

        state
      }
    }
  }
}