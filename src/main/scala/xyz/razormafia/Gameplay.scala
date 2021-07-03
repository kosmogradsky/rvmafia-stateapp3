package xyz.razormafia

import scala.annotation.tailrec
import scala.collection.immutable.SeqMap
import scala.util.chaining.scalaUtilChainingOps

private case class Gameplay(
                             round: Int,
                             alivePlayerNumbers: List[Int],
                             playersByNumber: Map[Int, Gameplay.Player],
                             phase: Phase
                           ) {
  final val playerCount = 10

  lazy val next: Gameplay = {
    phase match {
      case Phase.ContractNight => afterContractNight
      case day: Phase.Day => afterSpeech(day)
      case Phase.NormalDelay(nextPhase) =>
        copy(
          phase = nextPhase
        )
      case Phase.Nightfall =>
        copy(
          phase = Phase.MafiaShoots
        )
      case Phase.MafiaShoots =>
        copy(
          phase = Phase.GodfatherReveals
        )
      case Phase.GodfatherReveals =>
        copy(
          phase = Phase.SheriffReveals
        )
      case Phase.SheriffReveals => afterSheriffReveals
      case victimAnnouced: Phase.VictimAnnounced =>
        afterVictimAnnounced(victimAnnouced)
      case Phase.VictimThinks(victimKey) =>
        copy(
          phase = Phase.VictimSpeaks(victimKey)
        )
      case Phase.VictimSpeaks(_) | Phase.ShotMissAnnounced =>
        nextRound
      case votingAnnounced: Phase.VotingAnnounced =>
        afterVotingAnnounced(votingAnnounced)
      case voting: Phase.Voting =>
        afterVoting(voting)
      case exilesAnnounced: Phase.ExilesAnnounced =>
        afterExilesAnnounced(exilesAnnounced)
      case exileSpeaks: Phase.ExileSpeaks =>
        afterExileSpeaks(exileSpeaks)
      case votingSummary: Phase.VotingSummary =>
        afterVotingSummary(votingSummary)
      case splitAnnounced: Phase.SplitAnnounced =>
        afterSplitAnnounced(splitAnnounced)
      case spliteeSpeaks: Phase.SpliteeSpeaks =>
        afterSpliteeSpeaks(spliteeSpeaks)
      case catapulting: Phase.Catapulting =>
        afterCatapulting(catapulting)
    }
  }

  lazy val afterContractNight: Gameplay = {
    val speakerNumber::nextSpeakerNumbers = alivePlayerNumbers;

    copy(
      phase = Phase.NormalDelay(
        Phase.Day(
          speakerNumber,
          nextSpeakerNumbers
        )
      )
    )
  }

  lazy val nominatedSpeakerNumbers: List[Int] = {
    val nominees = for {
      playerNumber <- alivePlayerNumbers
      nomineeNumber <- playersByNumber.apply(playerNumber).nominated
    } yield (nomineeNumber)

    nominees.distinct
  }

  def afterSpeech(day: Phase.Day): Gameplay = {
    day.nextSpeakerNumbers match {
      case nextSpeakerNumber::otherSpeakerNumbers =>
        copy(
          phase = Phase.Day(nextSpeakerNumber, otherSpeakerNumbers)
        )
      case Nil =>
        copy(
          phase =
            if (nominatedSpeakerNumbers.isEmpty)
              Phase.Nightfall
            else
              Phase.VotingAnnounced(
                nominatedSpeakerNumbers,
                VotingStage.AgainstSpeakers
              )
        )
    }
  }

  lazy val aliveKillers: List[Gameplay.Player] = {
    alivePlayerNumbers
      .flatMap(
        playerNumber => {
          val player = playersByNumber(playerNumber)

          if (player.isKiller)
            Some(player)
          else
            None
        }
      )
  }

  lazy val victimNumberOption: Option[Int] = {
    val potentialVictims = aliveKillers
      .map(_.shot.getOrElse(0))

    val victimAsInt = potentialVictims.reduce(
      (prevVictim, victim) =>
        if ((prevVictim & victim) == victim)
          victim
        else
          0
    )

    if (alivePlayerNumbers.contains(victimAsInt))
      Some(victimAsInt)
    else
      None
  }

  lazy val afterSheriffReveals: Gameplay = {
    victimNumberOption match {
      case None =>
        copy(
          phase = Phase.NormalDelay(Phase.ShotMissAnnounced)
        )
      case Some(victimNumber) =>
        copy(
          phase = Phase.VictimAnnounced(victimNumber)
        )
    }
  }

  def afterVictimAnnounced(victimAnnounced: Phase.VictimAnnounced): Gameplay = {
    if (round == 0)
      copy(
        phase = Phase.VictimThinks(victimAnnounced.victimNumber)
      )
    else
      copy(
        phase = Phase.VictimSpeaks(victimAnnounced.victimNumber)
      )
  }

  lazy val nextRoundAlivePlayerNumbers: List[Int] = {
    alivePlayerNumbers.tail :+ alivePlayerNumbers.head
  }

  lazy val nextRound: Gameplay = {
    copy(
      round = round + 1,
      phase = Phase.Day(
        alivePlayerNumbers.head,
        alivePlayerNumbers.tail
      )
    )
  }

  def announceExiles(exiles: List[Int]) = {
    copy(
      alivePlayerNumbers = nextRoundAlivePlayerNumbers,
      phase = Phase.ExilesAnnounced(exiles)
    )
  }

  def afterVotingAnnounced(
                          phase: Phase.VotingAnnounced
                          ): Gameplay = {
    val nomineeNumber::nextNomineeNumbers = phase.nomineeNumbers

    if (nextNomineeNumbers.isEmpty)
      announceExiles(nomineeNumber::List.empty)
    else
      copy(
        phase = Phase.Voting(
          nomineeNumber,
          nextNomineeNumbers,
          phase.stage,
          Map()
        )
      )
  }

  def afterVoting(voting: Phase.Voting): Gameplay = {
    if (voting.nextNomineeNumbers.length == 1) {
      val lastNomineeNumber = voting.nextNomineeNumbers.head
      val finalVotes = alivePlayerNumbers
        .map(
          playerNumber => (
            playerNumber,
            voting.votes
              .get(playerNumber)
              .getOrElse(lastNomineeNumber)
          )
        )
        .toMap

      copy(
        phase = Phase.VotingSummary(
          lastNomineeNumber,
          finalVotes,
          voting.stage
        )
      )
    } else {
      copy(
        phase = Phase.Voting(
          voting.nextNomineeNumbers.head,
          voting.nextNomineeNumbers.tail,
          voting.stage,
          voting.votes
        )
      )
    }
  }

  def afterExilesAnnounced(exilesAnnounced: Phase.ExilesAnnounced) = {
    val exileNumber::nextExileNumbers = exilesAnnounced.exileNumbers

    copy(
      phase = Phase.ExileSpeaks(
        exileNumber,
        nextExileNumbers
      )
    )
  }

  def afterPlayerDies(playerNumber: Int): Gameplay = {
    copy(
      alivePlayerNumbers = alivePlayerNumbers
        .filter(_ != playerNumber)
    )
  }

  def afterExileSpeaks(exileSpeaks: Phase.ExileSpeaks) = {
    exileSpeaks.nextExileNumbers match {
      case nextExileNumber::otherExileNumbers =>
        afterPlayerDies(exileSpeaks.exileNumber).copy(
          phase = Phase.ExileSpeaks(
            nextExileNumber,
            otherExileNumbers
          )
        )
      case Nil =>
        afterPlayerDies(exileSpeaks.exileNumber).copy(
          phase = Phase.Nightfall
        )
    }
  }

  def getVotingResult(
                     votingSummary: Map[Int, Int]
                     ): (Int, List[Int]) = {
    val nomineeNumbersByVotesCount = alivePlayerNumbers
      .map(votingSummary(_))
      .foldLeft(Map.empty[Int, Int])(
        _.updatedWith(_)(_.map(_ + 1).orElse(Some(1)))
      )
      .foldLeft(Map.empty[Int, Set[Int]]) {
        case (acc, (nomineeNumber, votesCount)) =>
          acc
            .updatedWith(votesCount)(
              _
                .map(_.incl(nomineeNumber))
                .orElse(Some(Set(nomineeNumber)))
            )
      }
    val leadersSet = nomineeNumbersByVotesCount(
      nomineeNumbersByVotesCount.keys.max
    )
    val leadersOrdered = nominatedSpeakerNumbers
      .filter(leadersSet.contains(_))

    (leadersOrdered.head, leadersOrdered.tail)
  }

  def afterVotingSummary(votingSummary: Phase.VotingSummary) = {
    getVotingResult(votingSummary.votes) match {
      case (exile, Nil) =>
        announceExiles(exile::List.empty)
      case (splitee, nextSplitees) =>
        votingSummary.stage match {
          case VotingStage.AgainstSpeakers =>
            copy(
              phase = Phase.SplitAnnounced(splitee::nextSplitees)
            )
          case VotingStage.AgainstSplitees =>
            copy(
              phase = Phase.Catapulting(
                splitee::nextSplitees,
                Set()
              )
            )
        }

    }
  }

  def afterSplitAnnounced(splitAnnounced: Phase.SplitAnnounced) = {
    val spliteeNumber::nextSpliteeNumbers = splitAnnounced.spliteeNumbers

    copy(
      phase = Phase.SpliteeSpeaks(
        spliteeNumber,
        nextSpliteeNumbers,
        splitAnnounced.spliteeNumbers
      )
    )
  }

  def afterSpliteeSpeaks(spliteeSpeaks: Phase.SpliteeSpeaks) = {
    spliteeSpeaks.nextSpliteeNumbers match {
      case nextSpliteeNumber::otherSpliteeNumbers =>
        copy(
          phase = Phase.SpliteeSpeaks(
            nextSpliteeNumber,
            otherSpliteeNumbers,
            spliteeSpeaks.spliteeNumbers
          )
        )
      case Nil =>
        copy(
          phase = Phase.VotingAnnounced(
            spliteeSpeaks.spliteeNumbers,
            VotingStage.AgainstSplitees
          )
        )
    }
  }

  def afterCatapulting(catapulting: Phase.Catapulting) = {
    if (catapulting.votes.size > alivePlayerNumbers.length / 2)
      copy(
        phase = Phase.ExilesAnnounced(catapulting.splitees)
      )
    else
      copy(
        phase = Phase.Nightfall
      )
  }

  def nominate(
              nominatorNumber: Int,
              nomineeNumber: Int
              ): Pair[Gameplay, Boolean] = {
    playersByNumber
      .apply(nominatorNumber)
      .nominate(nomineeNumber)
      .mapFirst(
        nominator =>
          copy(
            playersByNumber = playersByNumber
              .updated(nominatorNumber, nominator)
          )
      )
  }

  def vote(voterNumber: Int) = {
    phase match {
      case voting: Phase.Voting =>
        if (alivePlayerNumbers.contains(voterNumber))
          copy(
            phase = Phase.Voting(
              voting.nomineeNumber,
              voting.nextNomineeNumbers,
              voting.stage,
              voting.votes.updatedWith(voterNumber) {
                _.orElse(Some(voting.nomineeNumber))
              }
            )
          )
        else
          this
      case _ =>
        this
    }
  }
}

object Gameplay {
  case class Player(
                   role: PlayerRole,
                   fouls: Fouls,
                   nominated: Option[Int],
                   shot: Option[Int]
                   ) {
    val isKiller = (
      role == PlayerRole.Mafia ||
      role == PlayerRole.Godfather
    )

    def nominate(nomineeNumber: Int): Pair[Player, Boolean] = {
      if (nominated.isEmpty)
        Pair(
          copy(
            nominated = Some(nomineeNumber)
          ),
          true
        )
      else
        Pair(this, false)
    }
  }

  object Player {
    def create(playerNumber: Int, role: PlayerRole) = {
      val player = Player(
        role = role,
        fouls = Fouls.Zero,
        nominated = None,
        shot = None
      )

      (playerNumber, player)
    }
  }

  def create(playerRoles: List[(Int, PlayerRole)]): Gameplay = {
    Gameplay(
      round = 0,
      alivePlayerNumbers = playerRoles.map(_._1),
      playersByNumber = playerRoles
        .map(Player.create)
        .pipe(Map.from),
      phase = Phase.ContractNight
    )
  }
}