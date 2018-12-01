package com.github.nickball.adventcode.y2015

import com.github.nickball.adventcode.Day

object Day01 extends Day(1) {

  def doSolutionA(input: String): String = {
    //map then sum(reduce)
    input.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).sum.toString
  }

  def doSolutionB(input: String): String = {
    var next = 0
    val index = input.map(c => {
      next += (if (c == '(') 1 else if (c == ')') -1 else 0)
      next
    }).indexOf(-1) + 1
    index.toString
  }
}
