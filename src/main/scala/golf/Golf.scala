package golf

import scala.collection.{IndexedSeqView, MapView, SeqView, View}
import scala.io.{BufferedSource, Source}
import scala.util.Using
import scala.util.Random


object Golf:

  case class Player (name: String, mean: Double, stdDev: Double)

  object Player:

    def fromString (str: String): Player =
      val (name, paramStr) = str.splitAt (26)
      val Array (meanStr, stdStr) = paramStr.trim.split ("\t")

      val mean = meanStr.trim.toDouble
      val std = stdStr.trim.toDouble

      Player (name.trim, mean, std)

  case class PlayerRound (player: Player, roundScore: Int)
  case class PlayerRounds (player: Player, roundScores: Array [Int]):
    def playerRound (round: Int): PlayerRound =
      PlayerRound (player, roundScores (round))

  object PlayerRounds:
    def apply (player: Player, number: Int): PlayerRounds =
      new PlayerRounds (player, (1 to number).map (_ => roundScoreFromPlayer (player)).toArray)

  case class Score (win: Double, top5: Double)
  case class ScoredPlayer (player: Player, score: Score)

  case class PlayerScores (player: Player, scores: Array [Score])

  def roundScoreFromPlayer (player: Player): Int =
    (Random.nextGaussian * player.stdDev + player.mean).toInt

  def rankedScores (players: IndexedSeqView [PlayerRound], rank: Int): List [(Int, View [Player])] =
    val byScore = players.groupMap (sp => sp.roundScore) (_.player).toList
    val sortedScores = byScore.sortBy (i => i._1)

    sortedScores
      .foldLeft (List.empty [(Int, View [Player])]) {
        case (List (), rhs) => List ((rhs._2.size, rhs._2))
        case (agg, rhs) if -1 < agg.last._1 && agg.last._1 < rank  => agg :+ (agg.last._1 + rhs._2.size, rhs._2)
        case (agg, rhs) => agg :+ (-1, rhs._2)
      }

  enum Rank:
    case winner, top5, both

  def getWeight (scoredPlayers: (Int, View [Player]), forRank: Rank): View [ScoredPlayer] =
    val score = 1.0 / scoredPlayers._2.size.toDouble
    forRank match
      case Rank.winner => scoredPlayers._2.map (s => ScoredPlayer (s, Score (score, 1.0)))
      case Rank.top5 => scoredPlayers._2.map (s => ScoredPlayer (s, Score (0.0, score)))
      case Rank.both => scoredPlayers._2.map (s => ScoredPlayer (s, Score (score, score)))

  def computeScores (rankedScores: List [(Int, View [Player])]): View [ScoredPlayer] =
    // score the winners
    rankedScores match
      case Nil => View.empty [ScoredPlayer]
      case List (scores) =>
        if scores._2.size < 5 then
          getWeight (scores, Rank.winner)
        else
          getWeight (scores, Rank.both)
      case _ =>
        val (first5, unranked) = rankedScores.partition (_._1 > -1)
        val winners = getWeight (first5.head, Rank.winner)
        val fifth = getWeight (first5.last, Rank.top5)
        val other3 = first5.tail.dropRight (1).flatMap (x => x._2.map (x => ScoredPlayer (x, Score (0.0, 1.0))))
        val greatUnwashed = unranked.flatMap (x => x._2.map (ScoredPlayer (_, Score (0.0, 0.0))))
        winners ++ other3 ++  fifth ++ greatUnwashed
  def aggregate (scoresByPlayer: MapView [Player, Array [Score]]): View [(Player, Score)] =
    for
      (player, scores) <- scoresByPlayer
    yield
      val totals = scores.foldLeft (Score (0.0, 0.0)) { (agg, rhs) => Score (agg.win + rhs.win, agg.top5 + rhs.top5)}
      val count = scores.length.toDouble
      (player, Score (totals.win / count, totals.top5 / count))

  @main
  def run (): Unit =
    val input = Source.fromFile ("data/ratings.txt")
    val numSims = 500

    val players: Array [Player] =
      for
        line <- input.getLines.toArray.tail
      yield
        Player.fromString (line)

    for
      // exp <- 20 to 26
      exp <- 1 to 12
      ns = 2 << exp
    yield
      val matches = players.map (PlayerRounds (_, ns))
      val scores = players.map (player => (player, Array.ofDim [Score] (ns))).toMap

      val output =
        for
          i <- 0 until ns
        yield
          val ranked = rankedScores (matches.map (_.playerRound (i)).view, 5)
          val scored = computeScores (ranked)

          scored.foreach: item =>
            scores (item.player).update (i, item.score)
          scored.map (sp => sp.player -> sp.score).toMap

      val result = aggregate (scores.view)

      println (s"Number of simulations: $ns:")
      result.foreach (r => println (s"\t$r"))

