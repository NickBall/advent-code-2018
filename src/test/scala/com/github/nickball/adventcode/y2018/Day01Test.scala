package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day01Test extends DayTest(Day01) {
  override protected def TableA(): TableFor2[String, AnyVal] = Table(
    ("in", "out"),
    ("+1\n-2\n+3\n+1", 3),
    ("+1\n+1\n+1", 3),
    ("+1\n+1\n-2", 0),
    ("-1\n-2\n-3", -6)
  )

  override protected def TableB(): TableFor2[String, AnyVal] = Table(
    ("in", "out"),
    ("+1\n-2\n+3\n+1", 2),
    ("+1\n-1", 0),
    ("+3\n+3\n+4\n-2\n-4", 10),
    ("-6\n+3\n+8\n+5\n-6", 5),
    ("+7\n+7\n-2\n-7\n-4", 14)
  )
}
