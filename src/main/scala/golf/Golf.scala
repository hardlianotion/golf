package golf

import scala.io.{BufferedSource, Source}
import scala.util.Using

import scala.util.Random


object Golf:

  case class Player (name: String, mean: Double, stdDev: Double)
  case class PlayerResult (player: Player, result: Int)
  case class ScoredPlayer (player: Player, score: Score)
  case class Score (win: Double, top5: Double)

  object Player:

    def fromString (str: String): Player =
      val (name, paramStr) = str.splitAt (26)
      val Array (meanStr, stdStr) = paramStr.trim.split ("\t")

      val mean = meanStr.trim.toDouble
      val std = stdStr.trim.toDouble

      Player (name.trim, mean, std)


  def roundScoreFromPlayer (player: Player): Int =
    (Random.nextGaussian * player.stdDev + player.mean).toInt

  def rankedScores (players: List [PlayerResult], rank: Int): List [(Int, List [Player])] =
    val byScore = players.groupMap (sp => sp.result) (sp => sp.player).toList
    val sortedScores = byScore.sortBy (i => i._1)

    sortedScores
      .foldLeft (List.empty [(Int, List [Player])]) {
        case (List (), rhs) => List ((rhs._2.length, rhs._2))
        case (agg, rhs) if -1 < agg.last._1 && agg.last._1 < rank  => agg :+ (agg.last._1 + rhs._2.length, rhs._2)
        case (agg, rhs) => agg :+ (-1, rhs._2)
      }

  def computeScores (rankedScores: List [(Int, List [Player])]): List [ScoredPlayer] =
    // score the winners
    rankedScores match
      case Nil => List.empty [ScoredPlayer]
      case List (scores) =>
        if scores._2.length < 5 then
          getWeight (scores, Rank.winner)
        else
          getWeight (scores, Rank.both)
      case _ =>
        val (first5, unranked) = rankedScores.partition (_._1 > -1)
        val winners = getWeight (first5.head, Rank.winner)
        val fifth = getWeight (first5.last, Rank.top5)
        val other3 = first5.tail.dropRight (1).flatMap (x => x._2.map (x => ScoredPlayer (x, Score (0.0, 1.0))))
        val greatUnwashed = unranked.flatMap (x => x._2.map (ScoredPlayer (_, Score (0.0, 0.0))))
        winners :++ other3 :++  fifth :++ greatUnwashed

  enum Rank:
    case winner, top5, both

  def getWeight (scoredPlayers: (Int, List [Player]), forRank: Rank): List [ScoredPlayer] =
    val score = 1.0 / scoredPlayers._2.length.toDouble
    forRank match
      case Rank.winner => scoredPlayers._2.map (s => ScoredPlayer (s, Score (score, 1.0)))
      case Rank.top5 => scoredPlayers._2.map (s => ScoredPlayer (s, Score (0.0, score)))
      case Rank.both => scoredPlayers._2.map (s => ScoredPlayer (s, Score (score, score)))

  def aggregate (scoresByPlayer: Map [Player, List [Score]]): Map [Player, Score] =
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

    val players: List [Player] =
      for
        line <- input.getLines.toList.tail
      yield
        Player.fromString (line)

    for
      exp <- 12 to 14
      ns = 2 << exp
    yield
      val output =
        for
          _ <- 1 to ns
        yield
          val scores = players.map : player =>
            PlayerResult (player, roundScoreFromPlayer (player))
          val ranked = rankedScores (scores, 5)
          val scored = computeScores (ranked)
          // switch to a map
          scored.map (sp => sp.player -> sp.score).toMap

      val sims = output.foldLeft (players.map ((_, List.empty [Score])).toMap) { (agg, rhs) =>
        agg.keys.map (k => k -> agg.apply (k).appended (rhs (k))).toMap
      }

      val result = aggregate (sims)


      //sims.foreach (println)
      println (s"Number of simulations: $ns:")
      result.foreach (r => println (s"\t$r"))

    //summaries.foreach (println)

