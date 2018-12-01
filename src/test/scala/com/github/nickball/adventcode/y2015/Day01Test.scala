package com.github.nickball.adventcode.y2015

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day01Test extends DayTest(Day01) {

  /*
  (()) and ()() both result in floor 0.
  ((( and (()(()( both result in floor 3.
  ))((((( also results in floor 3.
  ()) and ))( both result in floor -1 (the first basement level).
  ))) and )())()) both result in floor -3.
  */
  override protected def TableA(): TableFor2[String, AnyVal] = Table(
    ("input", "output"),
    ("(())", 0),
    ("()()", 0),
    ("((( ", 3),
    ("(()(()( ", 3),
    ("))(((((", 3),
    ("())", -1),
    ("))(", -1),
    (")))", -3),
  )

  /*
  ) causes him to enter the basement at character position 1.
  ()()) causes him to enter the basement at character position 5.
  */
  override protected def TableB(): TableFor2[String, AnyVal] = Table(
    ("input", "output"),
    (")", 1),
    ("()())", 5),
  )
}
