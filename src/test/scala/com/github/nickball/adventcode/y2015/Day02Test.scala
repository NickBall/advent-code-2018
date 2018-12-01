package com.github.nickball.adventcode.y2015

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day02Test extends DayTest(Day02) {
  override protected def TableA(): TableFor2[String, AnyVal] = Table(
    ("input", "output"),
    ("2x3x4", 58),
    ("1x1x10", 43)
  )

  override protected def TableB(): TableFor2[String, AnyVal] = Table(
    ("input", "output"),
    ("2x3x4", 34),
    ("1x1x10", 14)
  )
}
