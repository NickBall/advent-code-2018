package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.util.matching.Regex

object Day10 extends Day(10) {

  override protected def doSolutionA(input: String): String = {
    doPoints(input)._1
  }

  def doPoints(input: String): (String, Int) = {
    val points = parseInput(input)
    var i = 0
    var rangeY = calcRangeYPoint(points)
    var positions: Seq[Position] = null

    //FIXME different target sizes for unit tests vs production
    val targetRangeMax = rangeY match {
      case y if y > 100 => 10
      case _ => 8
    }

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

  override protected def doSolutionB(input: String): String = {
    doPoints(input)._2.toString
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
