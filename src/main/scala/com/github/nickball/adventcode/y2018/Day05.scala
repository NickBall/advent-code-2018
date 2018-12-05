package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.annotation.tailrec

object Day05 extends Day(5){
  override protected def doSolutionA(input: String) : String = {
    polymerJoin(input, Int.MaxValue).toString
  }

  @tailrec
  def polymerJoin(input: String, found: Int): Int = found match {
    case 0  => input.length //base case is no matches found on previous iteration
    case _ =>
      //track number of matches found in current iteration
      var matches = 0
      val sb = new StringBuilder(input)
      //TODO switch to regex expression?
      for (i <- 0 until input.length - 1) {
        val first = sb(i)
        val second = sb(i + 1)
        if (first != second && first.toLower == second.toLower) {
          matches += 1
          //Replace with placeholder for now to preserve iteration index
          sb.update(i, '0')
          sb.update(i + 1, '0')
        }
      }
      //Strip out the '0' "null characters", recurse
      polymerJoin(sb.toString.filterNot(_.equals('0')), matches)
  }

  override protected def doSolutionB(input: String) = ???
}
