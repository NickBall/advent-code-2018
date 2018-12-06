package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day06Test extends DayTest(Day06){
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9", 17),
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9", 16),
  )
}
