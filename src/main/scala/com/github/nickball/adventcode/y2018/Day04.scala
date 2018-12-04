package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day
import com.github.nickball.adventcode.common.collection.ListMultiMap

import scala.collection.mutable
import scala.util.matching.Regex

object Day04 extends Day(4) {

  override protected def doSolutionA(input: String): String = {
    val naps = buildNapMap(input)

    //Find guard with most minutes
    val guard = naps.mapValues(_.size).toSeq.sortBy(_._2).reverse.head._1
    //Find guard's most frequent minute
    val minute = naps(guard).groupBy(_.toInt).mapValues(_.size).toSeq.sortBy(_._2).reverse.head._1

    (guard * minute).toString
  }

  private def buildNapMap(input: String): mutable.HashMap[Int, mutable.MutableList[Int]] with ListMultiMap[Int, Int] = {
    val patternGuard: Regex = """.*Guard #(\d+) begins shift""".r
    val patternWakes: Regex = """.*\d+:(\d+)\] wakes up""".r
    val patternSleeps: Regex = """.*\d+:(\d+)\] falls asleep""".r

    //Lexical sort should do the trick
    val sorted = input.split("\n").sorted(Ordering.String)

    //Track guard asleep minutes - not using range since we'll need to explode/flatten it anyway
    val naps = new mutable.HashMap[Int, mutable.MutableList[Int]] with ListMultiMap[Int, Int]

    var curGuard = 0
    var rangeStart = -1

    //Loop through sleep logs, transform to guard-index range
    sorted.foreach {
      case patternGuard(guardNum) =>
        curGuard = guardNum.toInt
        rangeStart = -1
      case patternSleeps(sleepMin) =>
        rangeStart = sleepMin.toInt
      case patternWakes(wakeMin) =>
        (rangeStart until wakeMin.toInt).foreach(naps.addBinding(curGuard, _))
    }

    naps
  }

  override protected def doSolutionB(input: String): String = {
    val naps = buildNapMap(input)

    var bestGuard = -1
    var bestMinute = -1
    var bestCount = -1
    //find guard with most frequent minute
    naps.foreach(i => {
      val myMax = i._2.groupBy(_.toInt).mapValues(_.size).toSeq.sortBy(_._2).reverse.head
      if (myMax._2 > bestCount) {
        bestGuard = i._1
        bestCount = myMax._2
        bestMinute = myMax._1
      }
    })

    (bestGuard * bestMinute).toString
  }
}
