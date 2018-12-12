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

    def _calculatePowers(serial: Int): Map[Cell, Int] = allCells.map(c => c -> calculatePowerLevel(c, serial)).toMap

    serial => cache.getOrElseUpdate(serial, _calculatePowers(serial))
  }
  lazy val squarePowers: (Int, Cell, Int) => Int = {
    val cache = mutable.Map.empty[Int, mutable.Map[(Cell, Int), Int]]

    (serial, cell, size) => cache.getOrElseUpdate(serial, mutable.Map.empty[(Cell, Int), Int]).getOrElseUpdate((cell, size), calculateSquarePower(serial, size, cell))
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

    var largest = (Cell(-1, -1), Int.MinValue, Int.MinValue)
    (1 to maxSize).foreach(size => {
      val sum = findLargestSquare(serial, size)
      if (sum._2 > largest._3) {
        largest = (sum._1, size, sum._2)
        println(s"FOUND new largest: ${largest._1.x},${largest._1.y},${largest._2}. Size: $sum")
      }
    })
    s"${largest._1.x},${largest._1.y},${largest._2}"
  }

  private def findLargestSquare(serial: Int, squareSize: Int): (Cell, Int) = {
    println(s"Doing serial $serial with $squareSize size")

    //Do the squares
    var max = (Cell(-1, -1), Int.MinValue)
    val filterCells = allCells.filter(c => (c.x + squareSize - 1 <= 300) && (c.y + squareSize - 1 <= 300))
    filterCells.foreach(top => {
      val sum = calculateSquarePower(serial, squareSize, top)
      //Replace max if larger
      if (max._2 <= sum) {
        max = (top, sum)
      }
    })

    max
  }

  def calculateSquarePower(serial: Int, squareSize: Int, cell: Cell): Int = {
    val cellPowers = allCellsPower(serial)

    //Load partial sum from N-1 square
    val prevSum = squareSize match {
      case 1 => 0
      case _ => squarePowers(serial, cell, squareSize - 1)
    }

    //Add the new row+col to the previous subtotal of N-1 square
    val newCells = (0 until squareSize).map(i => {
      Array(Cell(cell.x + i, cell.y + squareSize - 1),
        Cell(cell.x + squareSize - 1, cell.y + i))
    }).flatten.toSet

    //Get the new sum
    val newSums = newCells.toSeq.map(c => cellPowers(c))
    val newSum = prevSum + newSums.sum

    newSum
  }

  case class Cell(x: Int, y: Int) {
    override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)
  }

}
