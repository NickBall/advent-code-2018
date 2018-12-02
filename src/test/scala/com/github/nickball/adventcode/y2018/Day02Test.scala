package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day02Test extends DayTest(Day02) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("abcdef\n" +
      "bababc\n" +
      "abbcde\n" +
      "abcccd\n" +
      "aabcdd\n" +
      "abcdee\n" +
      "ababab", 12)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz", "fgij")
  )
}
