package com.github.nickball.adventcode.y2015

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day03Test extends DayTest(Day03) {
  override protected def TableA(): TableFor2[String, AnyVal] = Table(
    ("in", "out"),
    (">", 2),
    ("^>v<", 4),
    ("^v^v^v^v^v", 2)
  )

  override protected def TableB(): TableFor2[String, AnyVal] = Table(
    ("in", "out"),
    ("^v", 3),
    ("^>v<", 3),
    ("^v^v^v^v^v", 11)
  )
}
