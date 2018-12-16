package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day16Test extends DayTest(Day16) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]", 1)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    //No test cases :-(
  )
}
