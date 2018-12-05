package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day05Test extends DayTest(Day05) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("aA",0),
    ("abBA", 0),
    ("abAB", 4),
    ("aabAAB",6),
    ("dabAcCaCBAcCcaDA", 10)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("dabAcCaCBAcCcaDA", 4)
  )
}
