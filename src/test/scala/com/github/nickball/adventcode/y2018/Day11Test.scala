package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import com.github.nickball.adventcode.y2018.Day11.Cell
import org.scalatest.prop.{TableFor2, TableFor3}

class Day11Test extends DayTest(Day11) {

  override def doSolutionB: String => String = {
    input => Day11.SolutionB(input, 17)
  }

  def TablePowerLevel(): TableFor3[Cell, Int, Int] = Table(
    ("cell", "serial", "out"),
    (Cell(3, 5), 8, 4),
    (Cell(122, 79), 57, -5),
    (Cell(217, 196), 39, 0),
    (Cell(101, 153), 71, 4)
  )

  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("18", "33,45"),
    ("42", "21,61")
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("18", "90,269,16"),
    ("42", "232,251,12")
  )

  forAll(TablePowerLevel()) { (cell: Cell, serial: Int, expected: Int) =>
    test(s"Cell $cell with serial $serial should produce power level $expected") {
      assert(Day11.calculatePowerLevel(cell, serial) == expected)
    }
  }
}
