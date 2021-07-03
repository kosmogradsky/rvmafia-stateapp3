package xyz.razormafia

import scala.scalajs.js.annotation._

case class NominationEvent(
                          @JSExport gameId: String,
                          @JSExport userId: String
                          )