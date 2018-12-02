package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day
import scala.language.implicitConversions

object Day02 extends Day(2) {
  override protected def doSolutionA(input: String): String = {
    //Quick conversion from bool to int
    implicit def bool2int(b: Boolean):Int = if (b) 1 else 0

    val lines = input.split("\n")

    val sums = lines.map(line => {
      val lineChars = line.toSeq
      //build character frequency map
      val groups = lineChars.groupBy(identity).mapValues(_.size)
      //check if we have any chars with frequency 2 or 3
      (groups.exists(_._2 == 2), groups.exists(_._2 == 3))
    }).foldLeft(0, 0)((i, j) => (i._1 + j._1, i._2 + j._2))

    (sums._1 * sums._2).toString
  }

  override protected def doSolutionB(input: String): String = {
    val lines = input.split("\n")

    var found = ""
    lines.foreach(line => {
      val longest = lines.filterNot(i => i.equalsIgnoreCase(line)).map(l => findCommon(line, l)).sortBy(_.length).reverse.head
      if (longest.length > found.length) found = longest
    })
    found
  }

  private def findCommon(a: String, b: String): String = {
    if (a.length != b.length) throw new IllegalArgumentException("A length must equal B length")

    var matches = ""
    for (i <- 0 until a.length) {
      if (a.charAt(i).equals(b.charAt(i))) {
        matches += a.charAt(i)
      }
    }
    matches
  }
}
