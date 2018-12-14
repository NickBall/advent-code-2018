package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day13Test extends DayTest(Day13) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("/->-\\        \n" +
      "|   |  /----\\\n" +
      "| /-+--+-\\  |\n" +
      "| | |  | v  |\n" +
      "\\-+-/  \\-+--/\n" +
      "  \\------/   ", "7,3"),
    ("|\n" +
      "v\n" +
      "|\n" +
      "|\n" +
      "|\n" +
      "^\n" +
      "|", "0,3")
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("/>-<\\  \n" +
      "|   |  \n" +
      "| /<+-\\\n" +
      "| | | v\n" +
      "\\>+</ |\n" +
      "  |   ^\n" +
      "  \\<->/", "6,4")
  )
}
