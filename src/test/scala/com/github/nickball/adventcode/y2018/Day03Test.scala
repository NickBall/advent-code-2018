package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day03Test extends DayTest(Day03) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2", 4)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2", 3)
  )
}
