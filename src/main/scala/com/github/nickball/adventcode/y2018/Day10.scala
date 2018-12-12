package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.util.matching.Regex

object Day10 extends Day(10) {

  def SolutionA(input: String, targetRangeMax: Int): String = doSolutionA(input, targetRangeMax)

  def SolutionB(input: String, targetRangeMax: Int): String = doSolutionB(input, targetRangeMax)

  override protected def doSolutionA(input: String): String = doSolutionA(input, 10)

  protected def doSolutionA(input: String, targetRangeMax: Int): String = {
    doPoints(input, targetRangeMax)._1
  }

  override protected def doSolutionB(input: String): String = doSolutionB(input, 10)

  protected def doSolutionB(input: String, targetRangeMax: Int): String = {
    doPoints(input, targetRangeMax)._2.toString
  }

  def doPoints(input: String, targetRangeMax: Int): (String, Int) = {
    val points = parseInput(input)
    var i = 0
    var rangeY = calcRangeYPoint(points)
    var positions: Seq[Position] = null

    do {
      i += 1
      positions = points.map(p => p.increment()).map(p => p.position)
      rangeY = calcRangeYPos(positions)
    } while (rangeY > targetRangeMax)

    (drawPoints(positions), i)
  }

  def calcRangeYPoint(positions: Seq[Point]): Int = calcRangeYPos(positions.map(p => p.position))

  def calcRangeYPos(points: Seq[Position]): Int = {
    val yVals = points.map(p => p.y).sortBy(identity)
    val yMin = yVals.min
    val yMax = yVals.max
    yMax - yMin
  }

  def drawPoints(pos: Seq[Position]): String = {
    //Canvas size
    val minY = pos.map(p => p.y).minBy(identity) - 4
    val maxY = pos.map(p => p.y).maxBy(identity) + 4
    val minX = pos.map(p => p.x).minBy(identity) - 6
    val maxX = pos.map(p => p.x).maxBy(identity) + 6

    val sb = StringBuilder.newBuilder
    (minY to maxY).foreach(y => {
      (minX to maxX).foreach(x => {
        val cur = Position(x, y)
        val char = if (pos.contains(cur)) {
          '#'
        } else {
          '.'
        }
        sb.append(char)
      })
      sb.append('\n')
    })
    sb.toString()
  }

  def parseInput(input: String): Seq[Point] = {
    val pattern: Regex = """position=<\s*([-+]?\d+),\s*([-+]?\d+)> velocity=<\s*([-+]?\d+),\s*([-+]?\d+)>""".r

    input.split("\n").map {
      case pattern(posX, posY, velX, velY) => Point(Position(posX.toInt, posY.toInt), Velocity(velX.toInt, velY.toInt))
    }.toSeq
  }

  case class Position(x: Int, y: Int)

  case class Velocity(x: Int, y: Int)

  case class Point(var position: Position, velocity: Velocity) {
    def increment(): Point = {
      position = Position(position.x + velocity.x, position.y + velocity.y)
      this
    }
  }

}
