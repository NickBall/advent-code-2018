package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day09Test extends DayTest(Day09) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("9 players; last marble is worth 25 points", 32),
    ("10 players; last marble is worth 1618 points", 8317),
    ("13 players; last marble is worth 7999 points", 146373),
    ("17 players; last marble is worth 1104 points", 2764),
    ("21 players; last marble is worth 6111 points", 54718),
    ("30 players; last marble is worth 5807 points", 37305)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("9 players; last marble is worth 25 points", 22563),
  )
}
