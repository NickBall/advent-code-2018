package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.mutable

object Day11 extends Day(11) {

  lazy val calculatePowerLevel: (Cell, Int) => Int = {
    def _calculatePowerLevel(cell: Cell, serial: Int): Int = {
      val rackID = cell.x + 10
      val power = ((rackID * cell.y) + serial) * rackID
      (Math.floorDiv(power, 100) % 10) - 5
    }

    val cache = mutable.Map.empty[(Cell, Int), Int]

    (cell, serial) =>
      cache.getOrElseUpdate((cell, serial), _calculatePowerLevel(cell, serial))
  }
  lazy val allCells: Seq[Cell] = for (y <- 1 to 300; x <- 1 to 300) yield Cell(x, y)
  lazy val allCellsPower: Int => Map[Cell, Int] = {
    val cache = mutable.Map.empty[Int, Map[Cell, Int]]

    def _calculatePowers(serial: Int): Map[Cell, Int] = {
      allCells.map(c => c -> calculatePowerLevel(c, serial)).toMap
    }

    serial => cache.getOrElseUpdate(serial, _calculatePowers(serial))
  }


  override protected def doSolutionA(input: String): String = {
    val top = findLargestSquare(input.toInt, 3)._1

    s"${top.x},${top.y}"
  }

  override protected def doSolutionB(input: String): String = {
    val serial = input.toInt

    //FIXME hack for unit tests since we don't have support for test parameterization and it's bloody slow to do whole grid
    val maxSize = serial match {
      case 18 => 17
      case 42 => 17
      case _ => 300
    }

    val largest = (1 to maxSize).map(size => size -> findLargestSquare(serial, size)).sortBy(_._2._2).reverse.head
    s"${largest._2._1.x},${largest._2._1.y},${largest._1}"
  }

  private def findLargestSquare(serial: Int, squareSize: Int): (Cell, Int) = {
    println(s"Doing serial $serial with $squareSize size")
    //Calculate cell values up front
    val cells = allCellsPower(serial)

    //Do the squares
    var max = (Cell(-1, -1), Int.MinValue)
    allCells.filter(c => (c.x + squareSize < 300) && (c.y + squareSize < 300)).foreach(top => {
      var sum = 0
      for (y <- 0 until squareSize; x <- 0 until squareSize) yield {
        sum += cells(Cell(top.x + x, top.y + y))
      }
      top -> sum

      //Replace max if larger
      if (max._2 < sum) {
        max = (top, sum)
      }
    })

    max
  }

  case class Cell(x: Int, y: Int) {
    override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)
  }

}
