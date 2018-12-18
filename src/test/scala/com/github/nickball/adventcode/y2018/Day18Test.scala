package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day18Test extends DayTest(Day18) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    (".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.", 1147)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out")
    //No test case :-(
  )
}
