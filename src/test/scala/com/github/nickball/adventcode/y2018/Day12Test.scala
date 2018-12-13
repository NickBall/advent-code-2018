package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.DayTest
import org.scalatest.prop.{TableFor1, TableFor2}

class Day12Test extends DayTest(Day12) {

  override def doSolutionA: String => String = {
    input => Day12.doSolutionA(input, 20)
  }

  override protected def TableA(): TableFor2[String, Any] = Table(
    ("in", "out"),
    (testInput, 325)
  )

  override protected def TableB(): TableFor2[String, Any] = Table(
    ("in", "out"),
    (testInput, "999999999374")
  )

  def testInput = "initial state: #..#.#..##......###...###\n" +
    "\n" +
    "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n" +
    ".#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n" +
    "###.# => #\n####. => #"

  protected def TableGenerations(): TableFor1[String] = Table(
    "generations",
    "0: ...#..#.#..##......###...###...........",
    "1: ...#...#....#.....#..#..#..#...........",
    "2: ...##..##...##....#..#..#..##..........",
    "3: ..#.#...#..#.#....#..#..#...#..........",
    "4: ...#.#..#...#.#...#..#..##..##.........",
    "5: ....#...##...#.#..#..#...#...#.........",
    "6: ....##.#.#....#...#..##..##..##........",
    "7: ...#..###.#...##..#...#...#...#........",
    "8: ...#....##.#.#.#..##..##..##..##.......",
    "9: ...##..#..#####....#...#...#...#.......",
    "10: ..#.#..#...#.##....##..##..##..##......",
    "11: ...#...##...#.#...#.#...#...#...#......",
    "12: ...##.#.#....#.#...#.#..##..##..##.....",
    "13: ..#..###.#....#.#...#....#...#...#.....",
    "14: ..#....##.#....#.#..##...##..##..##....",
    "15: ..##..#..#.#....#....#..#.#...#...#....",
    "16: .#.#..#...#.#...##...#...#.#..##..##...",
    "17: ..#...##...#.#.#.#...##...#....#...#...",
    "18: ..##.#.#....#####.#.#.#...##...##..##..",
    "19: .#..###.#..#.#.#######.#.#.#..#.#...#..",
    "20: .#....##....#####...#######....#.#..##."
  )

  //Test each generation of example
  forAll(TableGenerations()) { (input: String) =>
    val inputs = input.split(": ")
    val gen = inputs(0).toInt
    //Cheat and use the calcSum from main code
    //Since the sample input is fixed at -3 offset, we use that instead of generation index
    val expected = Day12.calculateSum(inputs(1), -3)
    test(s"Generation $gen for example input should produce $expected") {
      //Need to use contains, due to padding of the input generations
      assert(Day12.doSolutionA(testInput, gen) == expected.toString)
    }
  }

  test(s"Input should be parsed correctly") {
    val actual = Day12.parseInput(testInput)
    assert(actual.state == "#..#.#..##......###...###")
    assert(actual.spread.length == testInput.split("\n").length - 2)

    assert(actual.spread(0).pattern == "...##")
    assert(actual.spread(0).result == '#')

    assert(actual.spread(6).pattern == ".####")
    assert(actual.spread(6).result == '#')
  }
}
