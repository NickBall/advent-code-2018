package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import com.github.nickball.adventcode.y2019.Day08
import org.scalatest.prop.TableFor2

class Day08Test extends DayTest(Day08) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2", 138)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2", 66)
  )
}
