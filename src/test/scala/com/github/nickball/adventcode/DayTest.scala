package com.github.nickball.adventcode

import org.scalatest.FunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

abstract class DayTest(day: Day) extends FunSuite with TableDrivenPropertyChecks {
  protected def TableA(): TableFor2[String, Any]

  protected def TableB(): TableFor2[String, Any]

  forAll(TableA()) { (input: String, output: Any) =>
    test(s"'$input' applied to A should output $output") {
      assert(day.SolutionA(input) == output.toString)
    }
  }

  forAll(TableB()) { (input: String, output: Any) =>
    test(s"'$input' applied to B should output $output") {
      assert(day.SolutionB(input) == output.toString)
    }
  }
}
