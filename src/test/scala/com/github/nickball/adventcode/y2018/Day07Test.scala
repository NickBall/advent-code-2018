package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day07Test extends DayTest(Day07) {
  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\n" +
      "Step A must be finished before step B can begin.\nStep A must be finished before step D can begin.\n" +
      "Step B must be finished before step E can begin.\nStep D must be finished before step E can begin.\n" +
      "Step F must be finished before step E can begin.", "CABDFE")
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    ("Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\n" +
      "Step A must be finished before step B can begin.\nStep A must be finished before step D can begin.\n" +
      "Step B must be finished before step E can begin.\nStep D must be finished before step E can begin.\n" +
      "Step F must be finished before step E can begin.", 15)
  )
}
