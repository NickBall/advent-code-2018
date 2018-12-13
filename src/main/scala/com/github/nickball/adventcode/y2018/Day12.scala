package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

object Day12 extends Day(12) {

  override protected def doSolutionA(input: String): String = doSolutionA(input, 20)

  def doSolutionA(input: String, generations: Int): String = {
    val parsed = parseInput(input)

    val plants = (0 until generations).foldLeft(parsed.state)((prev, _) => doGeneration(prev, parsed.spread))
    val sum = calculateSum(plants, -generations).toString
    println(s"Generation result: $sum . Plants: $plants . Offset: ${-generations}")
    sum
  }

  def parseInput(input: String): ParsedInput = {
    val lines = input.split("\n")
    val initialState = lines(0).split(": ")(1)

    val spreads = (2 until lines.length).map(lines(_)).map(l => l.split(" => ")).map(p => Spread(p(0), p(1)(0)))

    ParsedInput(initialState, spreads)
  }

  def calculateSum(input: String, offset: Int): Int = {
    (0 until input.length).map(i => {
      input(i) match {
        case '.' => 0
        case '#' => i + offset
      }
    }).sum
  }

  def doGeneration(prevGen: String, spreads: Seq[Spread]): String = {
    //Append for this generation
    val working = '.' + prevGen + '.'

    //Get character at a given position, allowing for out-of-range
    def getPosition(pos: Int): Char = {
      pos match {
        //default to empty for overflows
        case x if x < 0 => '.'
        case y if y >= working.length => '.'
        case _ => working(pos)
      }
    }

    //Generate the patterns of each plant for matching to the spread patterns
    val patterns = (0 until working.length).map(i => i -> {
      //Get string for pattern matching
      val center = working(i)
      val left = (-2 to -1).map(i + _).map(getPosition).foldLeft("")(_ + _)
      val right = (1 to 2).map(i + _).map(getPosition).foldLeft("")(_ + _)
      left + center + right
    })
    patterns.map(p => {
      //Look for matching spread pattern
      val spread = spreads.collectFirst { case spread if spread.pattern == p._2 => spread }
      spread.map(s => s.result).getOrElse('.')
    }).mkString
  }


  override protected def doSolutionB(input: String): String = {
    val parsed = parseInput(input)
    val simGenerations = 512
    val totalGenerations = 50000000000L

    var prevSum = calculateSum(parsed.state, 0)
    var diff = 0
    var i = 0
    //Generation diff stabilizes after a relatively small number of generations
    //Extrapolate that to the full generation count
    (0 until simGenerations).foldLeft(parsed.state)((prev, _) => {
      val gen = doGeneration(prev, parsed.spread)
      val sum = calculateSum(gen, -i)
      diff = sum - prevSum
      println(s"Gen $i. Sum: $sum. Prev: $prevSum. Diff: $diff")
      prevSum = sum
      i += 1
      gen
    })

    (prevSum + (totalGenerations - simGenerations - 1) * diff).toString
  }

  case class Spread(pattern: String, result: Char)

  case class ParsedInput(state: String, spread: Seq[Spread])

}
