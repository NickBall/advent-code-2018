package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.mutable

object Day18 extends Day(18) {
  override protected def doSolutionA(input: String): String = {
    //Mutate 10 times
    val finalMap = mutateN(inputToMap(input), 10)
    calculateResourceValue(finalMap).toString
  }

  private def calculateResourceValue(map: Array[Array[Char]]): Int = {
    map.flatten.count(_ == '|') * map.flatten.count(_ == '#')
  }

  private def inputToMap(input: String): Array[Array[Char]] = {
    val lines = input.split("\n")
    val map = Array.ofDim[Char](lines.length, lines.length)

    //Get original map
    (0 until lines.length).foreach(y => {
      (0 until lines(y).length).foreach(x => {
        map(x)(y) = lines(y)(x)
      })
    })
    map
  }

  protected def mutateN(map: Array[Array[Char]], times: Int): Array[Array[Char]] = {
    (1 to times).foldLeft(map)((m, _) => mutate(m))
  }

  protected def mutate(map: Array[Array[Char]]): Array[Array[Char]] = {
    val newMap = Array.ofDim[Char](map.length, map.length)
    map.indices.foreach(x => {
      map(x).indices.foreach(y => {
        val neighbors = findNeighbors(map, x, y)
        val res = map(x)(y) match {
          case '.' => doOpenAcre(neighbors)
          case '|' => doTrees(neighbors)
          case '#' => doLumberyard(neighbors)
        }
        newMap(x)(y) = res
      })
    })
    newMap
  }

  private def doOpenAcre(neighbors: Seq[Char]): Char = {
    neighbors.count(n => n == '|') match {
      case t if t >= 3 => '|'
      case _ => '.'
    }
  }

  private def doTrees(neighbors: Seq[Char]): Char = {
    neighbors.count(n => n == '#') match {
      case t if t >= 3 => '#'
      case _ => '|'
    }
  }

  private def doLumberyard(neighbors: Seq[Char]): Char = {
    if (neighbors.contains('#') && neighbors.contains('|')) {
      '#'
    } else {
      '.'
    }
  }

  private def findNeighbors(map: Array[Array[Char]], myX: Int, myY: Int): Seq[Char] = {
    var neighbors = mutable.MutableList.empty[Char]
    neighbors.sizeHint(8)
    (myX - 1 to myX + 1).foreach(x => {
      (myY - 1 to myY + 1).foreach(y => {
        if ((x >= 0 && x < map.length) && (y >= 0 && y < map(x).length) && !(x == myX && y == myY)) {
          //Make sure we are in bounds and ignore origin point
          neighbors += map(x)(y)
        }
      })
    })
    neighbors
  }

  override protected def doSolutionB(input: String): String = {
    //TODO figured out manually by looking at pattern, do automatically
    val patternStart = 431
    val patternEnd = 465
    val targetMin = 1000000000

    val patternPeriod = patternEnd - patternStart + 1
    var map = (1 until patternStart).foldLeft(inputToMap(input))((m, _) => mutate(m))
    val store = (0 until patternPeriod).map(i => {
      map = mutate(map)
      val score = calculateResourceValue(map)
      println(s"Minute $i. Score: $score")
      i -> score
    }).toMap

    val offset = (targetMin - patternStart) % patternPeriod
    println(s"Offset: $offset, score: ${store(offset)}")
    store(offset).toString
  }

  private def mapToString(map: Array[Array[Char]]): String = {
    val sb = StringBuilder.newBuilder
    map(0).indices.foreach(y => {
      map.indices.foreach(x => {
        sb.append(map(x)(y))
      })
      sb.append("\n")
    })
    sb.toString
  }
}
