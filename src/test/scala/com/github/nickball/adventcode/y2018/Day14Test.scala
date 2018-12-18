package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day14Test extends DayTest(Day14) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("5", "0124515891"),
    ("9", "5158916779"),
    ("18", "9251071085"),
    ("2018", "5941429882")
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("01245", "5"),
    ("51589", "9"),
    ("92510", "18"),
    ("59414", "2018")
  )
}
