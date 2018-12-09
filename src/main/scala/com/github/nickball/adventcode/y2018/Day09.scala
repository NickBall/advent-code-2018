package com.github.nickball.adventcode.y2018

import java.time.LocalTime

import com.github.nickball.adventcode.Day

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.matching.Regex

object Day09 extends Day(9) {
  override protected def doSolutionA(input: String): String = {
    val (playersCount, lastMarble) = parseInput(input)
    val highScore = playGame(playersCount, lastMarble)
    highScore.toString
  }

  override protected def doSolutionB(input: String): String = {
    val (playersCount, lastMarble) = parseInput(input)
    val highScore = playGame(playersCount, lastMarble * 100)
    highScore.toString
  }

  def playGame(playersCount: Int, lastMarble: Int): Long = {
    //Track marble circle
    val marbles = mutable.ArrayBuffer[Int](0)
    marbles.sizeHint(lastMarble)

    //Track player scores
    var scores = mutable.Map[Int, Long]().withDefault(_ => 0)
    //Track player turns
    var players = Queue[Int]()
    (1 to playersCount).foreach(p => players = players.enqueue(p))

    var current_i = 0
    (1 to lastMarble).foreach(marble => {
      if (lastMarble > 100000) {
        marble % (lastMarble / 10) match {
          case 0 => println(s"${LocalTime.now().toString}: Completed $marble of $lastMarble ...")
          case _ => //who cares
        }
      }

      //Get current player
      val (player, new_players) = players.dequeue
      players = new_players

      //Check if this is a "winning" marble ( mod 23)
      marble % 23 match {
        case 0 =>
          //Winner! Remove the -7th marble
          val removal = ((current_i - 7) + marbles.size) % marbles.size
          val marble_remove = marbles.remove(removal)
          current_i = removal % marbles.size

          //Calculate score of removed marble and current marble
          val score = marble + marble_remove
          scores = scores.updated(player, scores(player) + score)
        case _ =>
          //Add marble to circle
          val next = (current_i + 1) % marbles.size + 1
          next match {
            case x if x >= marbles.size => marbles.append(marble)
            case _ => marbles.insert(next, marble)
          }
          current_i = next
      }
      //Add player to end of queue for next turn
      players = players.enqueue(player)
    })

    //Find biggest score
    val highScore = scores.values.toSeq.sortBy(identity).reverse.head
    highScore
  }

  private def parseInput(input: String): (Int, Int) = {
    val pattern: Regex = """(\d+) players; last marble is worth (\d+) points""".r
    val (players_count, last_marble) = input match {
      case pattern(p, m) => (p.toInt, m.toInt)
    }
    (players_count, last_marble)
  }
}
