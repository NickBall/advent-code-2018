package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day
import com.github.nickball.adventcode.common.collection.DoublyCircularLinkedList

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
    val marbles = new DoublyCircularLinkedList[Int]()
    marbles.add(0)

    //Track player scores
    var scores = mutable.Map[Int, Long]().withDefault(_ => 0)
    //Track player turns
    var players = Queue[Int]()
    (1 to playersCount).foreach(p => players = players.enqueue(p))

    (1 to lastMarble).foreach(marble => {
      //Get current player
      val (player, new_players) = players.dequeue
      players = new_players

      //Check if this is a "winning" marble ( mod 23)
      marble % 23 match {
        case 0 =>
          //Winner! Remove the -7th marble
          (1 to 7).foreach(_ => marbles.prev())
          val marbleRemove = marbles.remove().get
          marbles.next()

          //Calculate score of removed marble and current marble
          val score = marble + marbleRemove
          scores = scores.updated(player, scores(player) + score)
        case _ =>
          //Add marble to circle
          marbles.next()
          marbles.add(marble)
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
