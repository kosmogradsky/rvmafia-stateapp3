package xyz.razormafia

import utest._

object PlaygroundTest extends TestSuite {
  val playground = State.initial
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
    .playgrounds("gameId")

  val tests = Tests {
    test("playgroundState") {
      test("startDistributing") {
        playground.next
      }

      test("distributing") {
        var testedPlayground = playground

        for (i <- 0 until 6) {
          testedPlayground = testedPlayground.next
        }

        val lastFiveRolesToDistribute = testedPlayground

        for (i <- 0 until 3) {
          testedPlayground = testedPlayground.next
        }

        val lastOneRoleToDistribute = testedPlayground

        println(lastFiveRolesToDistribute)
        println(lastOneRoleToDistribute)
      }

      test("playing") {
        var testedPlayground = playground

        for (i <- 0 until 11) {
          testedPlayground = testedPlayground.next
        }

        testedPlayground
      }
    }
  }
}
