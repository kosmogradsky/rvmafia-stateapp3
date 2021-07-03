package xyz.razormafia

import scala.util.Random

case class QueueEntry(orderToken: Int, orderTokenRange: Int)

object QueueEntry {
  def create(orderTokenRange: Int = 1024): QueueEntry = {
    QueueEntry(Random.between(1, orderTokenRange + 1), orderTokenRange)
  }
}