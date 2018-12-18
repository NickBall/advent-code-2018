package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day
import com.github.nickball.adventcode.common.collection.DoublyCircularLinkedList

object Day14 extends Day(14) {
  override protected def doSolutionA(input: String): String = {
    val numRecipes = input.toInt

    val scoreboard = new DoublyCircularLinkedList[Int]
    scoreboard.add(3)
    scoreboard.add(7)

    var curPositions = (0, 1)

    var r = 0
    while (r < numRecipes + 10) {
      val res = doRound(scoreboard, curPositions)
      curPositions = res
      r += 1
    }

    aggregateScore(scoreboard, numRecipes)
  }

  override protected def doSolutionB(input: String) : String = {
    val million = 1024000
    val scoreboard = new DoublyCircularLinkedList[Int]
    scoreboard.add(3)
    scoreboard.add(7)
    var curPositions = (0, 1)

    var r = 0
    var found = false
    while (!found && r <= 1000 * million) {
      val res = doRound(scoreboard, curPositions)
      curPositions = res
      r += 1

      //Check every million if we've found it
      if (r % million == 0) {
        scoreboard.first
        found = scoreboard.toSeq.mkString.indexOf(input) > 0
      }
    }

    scoreboard.first
    scoreboard.toSeq.mkString.indexOf(input).toString
  }

  def doRound(scoreboard: DoublyCircularLinkedList[Int], positions: (Int, Int)): (Int, Int) = {
    val elf1 = doElfScore(scoreboard, positions._1, "elf1")
    val elf2 = doElfScore(scoreboard, positions._2, "elf2")
    val elves = Seq(elf1, elf2)

    //Append new scores to scoreboard
    scoreboard.last
    val newScore = elves.sum
    if (newScore >= 10) {
      scoreboard.add(newScore / 10)
    }
    scoreboard.add(newScore % 10)

    //Calculate new positions
    val elfPos1 = doElfPos(scoreboard, positions._1, "elf1")
    val elfPos2 = doElfPos(scoreboard, positions._2, "elf2")

    (elfPos1, elfPos2)
  }

  def doElfScore(scoreboard: DoublyCircularLinkedList[Int], position: Int, elfName: String): Int = {
    val prevPosition = scoreboard.loadBookmark(elfName)
    prevPosition match {
      case None =>
        //Traverse manually to location
        scoreboard.first
        var i = 0
        while (i < position) {
          scoreboard.next()
          i += 1
        }
        scoreboard.get.get
      case s :Some[Int] =>
        s.get
    }
  }

  def doElfPos(scoreboard: DoublyCircularLinkedList[Int], position: Int, elfName: String): Int = {
    val prevPosition = scoreboard.loadBookmark(elfName)
    val curScore = prevPosition match {
      case None =>
        scoreboard.first
        var i = 0
        while (i < position) {
          scoreboard.next()
          i += 1
        }
        scoreboard.get.get
      case s: Some[Int] =>
        scoreboard.get.get
    }
    //Move prevScore places from start
    (0 until scoreboard.get.get + 1).foreach(_ => scoreboard.next)
    scoreboard.setBookmark(elfName)
    (position + curScore + 1) % scoreboard.size
  }

  def aggregateScore(scoreboard: DoublyCircularLinkedList[Int], input: Int): String = {
    //Reset scoreboard to first
    scoreboard.first

    //Move to position
    (0 until input).foreach(_ => scoreboard.next())

    //Get next 10
    (0 until 10).map(_ => {
      val value = scoreboard.get.get
      scoreboard.next()
      value
    }).mkString
  }

}
