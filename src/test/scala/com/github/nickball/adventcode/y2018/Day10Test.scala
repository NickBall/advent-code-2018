package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.TableFor2

class Day10Test extends DayTest(Day10) {

  override def doSolutionA: String => String = {
    (input) => Day10.SolutionA(input, 8)
  }

  override def doSolutionB: String => String = {
    (input) => Day10.SolutionB(input, 8)
  }

  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    (input,
      "......................\n" +
        "......................\n" +
        "......................\n" +
        "......................\n" +
        "......#...#..###......\n" +
        "......#...#...#.......\n" +
        "......#...#...#.......\n" +
        "......#####...#.......\n" +
        "......#...#...#.......\n" +
        "......#...#...#.......\n" +
        "......#...#...#.......\n" +
        "......#...#..###......\n" +
        "......................\n" +
        "......................\n" +
        "......................\n" +
        "......................\n")
  )

  def input: String = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\n" +
    "position=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\n" +
    "position=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\n" +
    "position=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\n" +
    "position=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\n" +
    "position=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\n" +
    "position=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\n" +
    "position=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\n" +
    "position=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\n" +
    "position=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\n" +
    "position=<-3,  6> velocity=< 2, -1>"

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    (input, 3)
  )
}
