package xyz.razormafia

enum Phase(val duration: Int) {
  case NormalDelay(nextPhase: Phase) extends Phase(3000)

  case ContractNight extends Phase(60000)

  case Day(
          speakerNumber: Int,
          nextSpeakerNumbers: List[Int]
          ) extends Phase(60000)

  case VotingAnnounced(
                      nomineeNumbers: List[Int],
                      stage: VotingStage
                      ) extends Phase(5000)

  case Voting(
             nomineeNumber: Int,
             nextNomineeNumbers: List[Int],
             stage: VotingStage,
             votes: Map[Int, Int]
             ) extends Phase(4000)

  case VotingSummary(
                    lastNomineeNumber: Int,
                    votes: Map[Int, Int],
                    stage: VotingStage
                    ) extends Phase(3000)

  case Nightfall extends Phase(3000)

  case MafiaShoots extends Phase(5000)

  case GodfatherReveals extends Phase(5000)

  case SheriffReveals extends Phase(5000)

  case VictimAnnounced(victimNumber: Int) extends Phase(3000)

  case VictimThinks(victimNumber: Int) extends Phase(20000)

  case VictimSpeaks(victimNumber: Int) extends Phase(40000)

  case ShotMissAnnounced extends Phase(3000)

  case ExilesAnnounced(
                      exileNumbers: List[Int]
                      ) extends Phase(3000)

  case ExileSpeaks(
                  exileNumber: Int,
                  nextExileNumbers: List[Int]
                  ) extends Phase(60000)

  case SplitAnnounced(spliteeNumbers: List[Int]) extends Phase(3000)

  case SpliteeSpeaks(
                    spliteeNumber: Int,
                    nextSpliteeNumbers: List[Int],
                    spliteeNumbers: List[Int]
                    ) extends Phase(60000)

  case Catapulting(
               splitees: List[Int],
               votes: Set[Int]
               ) extends Phase(4000)
}
