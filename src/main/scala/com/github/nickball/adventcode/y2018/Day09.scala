package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.immutable.Queue
import scala.util.matching.Regex

object Day09 extends Day(9) {
  override protected def doSolutionA(input: String): String = {
    val pattern: Regex = """(\d+) players; last marble is worth (\d+) points""".r
    val (players_count, last_marble) = input match {
      case pattern(p, m) => (p.toInt, m.toInt)
    }

    //Track marble circle
    var marbles = Vector[Int](0)
    //Track player scores
    var scores = Map[Int, Int]().withDefault(_ => 0)
    //Track player turns
    var players = Queue[Int]()
    (1 to players_count).foreach(p => players = players.enqueue(p))

    var current_i = 0
    (1 to last_marble).foreach(marble => {
      //Get current player
      val (player, new_players) = players.dequeue
      players = new_players

      //Check if this is a "winning" marble ( mod 23)
      marble % 23 match {
        case 0 =>
          //Winner! Remove the -7th marble
          val removal = ((current_i - 7) + marbles.size) % marbles.size
          val marble_remove = marbles(removal)
          marbles = marbles.filterNot(m => m == marble_remove)
          current_i = removal % marbles.size

          //Calculate score of removed marble and current marble
          val score = marble_remove + marble
          scores = scores.updated(player, scores(player) + score)
        //println(s"23! Player $player added $score to score. Removed marble $marble_remove. Current $current_i")
        case _ =>
          //Add marble to circle
          val next = (current_i + 1) % marbles.size
          val (front, back) = marbles.splitAt(next + 1)
          marbles = front ++ List(marble) ++ back
          current_i = next + 1
        //println(s"Player $player added $marble. New sequence: ${marbles.toString}. Current ${marbles(current_i)}")
      }
      //Add player to end of queue for next turn
      players = players.enqueue(player)
    })

    //Find biggest score
    scores.values.toSeq.sortBy(identity).reverse.head.toString
  }

  override protected def doSolutionB(input: String) = ???
}
