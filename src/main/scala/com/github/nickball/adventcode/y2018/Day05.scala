package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.annotation.tailrec

object Day05 extends Day(5){
  override protected def doSolutionA(input: String) : String = {
    @tailrec
    def polymerJoin(input: String, found: Int): Int = found match {
        case 0  => input.length //base case is no matches found on previous iteration
        case _ =>
          println(s"polymerJoin call. input len:${input.length}, prev found:${found}")
          //track number of matches found in current iteration
          var matches = 0
          val sb = new StringBuilder(input)
          //TODO switch to regex expression?
          for (i <- 0 until input.length - 1) {
            val first = sb(i)
            val second = sb(i + 1)
            //ugly but works! could switch with an ASCII code compare
            if ((first.toUpper == second && first.isLower && second.isUpper) || (first.toLower == second && first.isUpper && second.isLower)) {
              matches += 1
              //Replace with placeholder for now to preserve iteration index
              sb.update(i, '0')
              sb.update(i + 1, '0')
            }
          }
          //Strip out the '0' "null characters", recurse
          polymerJoin(sb.toString.filterNot(_.equals('0')), matches)
    }
    polymerJoin(input, Int.MaxValue).toString
  }

  override protected def doSolutionB(input: String) = ???
}
