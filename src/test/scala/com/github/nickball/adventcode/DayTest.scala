package com.github.nickball.adventcode

import org.scalatest.FunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

abstract class DayTest(day: Day) extends FunSuite with TableDrivenPropertyChecks {
  protected def TableA(): TableFor2[String, AnyVal]

  protected def TableB(): TableFor2[String, AnyVal]

  forAll(TableA()) { (input: String, output: AnyVal) =>
    test(s"'$input' applied to A should output $output") {
      assert(day.SolutionA(input) == output.toString)
    }
  }

  forAll(TableB()) { (input: String, output: AnyVal) =>
    test(s"'$input' applied to B should output $output") {
      assert(day.SolutionB(input) == output.toString)
    }
  }
}
