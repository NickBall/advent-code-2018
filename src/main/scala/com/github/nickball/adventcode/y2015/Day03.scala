package com.github.nickball.adventcode.y2015

import com.github.nickball.adventcode.Day

import scala.collection.mutable

object Day03 extends Day(3) {
  override protected def doSolutionA(input: String): String = {
    var cur = (0, 0)
    input.map(i => {
      cur = newCoordinates(cur, i)
      cur
    }).toSet[(Int, Int)].union(Set((0, 0))).size.toString
  }

  override protected def doSolutionB(input: String): String = {
    val coordinates = mutable.Set[(Int, Int)]()
    coordinates += ((0, 0))
    val indexed = input.zipWithIndex
    //Filter odd indexes only (for Santa)
    indexed.filter(_._2 % 2 == 1).scanLeft((0, 0)) { (cur, i) => {
      var x = newCoordinates(cur, i._1)
      coordinates += x
      x
    }
    }
    //Filter evens only (for Robo)
    indexed.filter(_._2 % 2 == 0).scanLeft((0, 0)) { (cur, i) => {
      var x = newCoordinates(cur, i._1)
      coordinates += x
      x
    }
    }
    coordinates.size.toString
  }

  private def newCoordinates(cur: (Int, Int), dir: Char): (Int, Int) = dir match {
    case '^' => (cur._1, cur._2 + 1)
    case 'v' => (cur._1, cur._2 - 1)
    case '>' => (cur._1 + 1, cur._2)
    case '<' => (cur._1 - 1, cur._2)
    case _ => (cur._1, cur._2)
  }
}
