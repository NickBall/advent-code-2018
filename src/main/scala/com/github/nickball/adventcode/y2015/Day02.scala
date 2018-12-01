package com.github.nickball.adventcode.y2015

import com.github.nickball.adventcode.Day

object Day02 extends Day(2) {
  override protected def doSolutionA(input: String): String = {
    input.split('\n').map(calcSurfaceArea).sum.toString
  }

  private def calcSurfaceArea(input: String): Int = {
    //Split each lwh pair
    val Array(l, w, h) = input.split("x").map(_.toInt)
    //Calc area of each dimension
    val areas = List(l * w, w * h, h * l)
    //Calculate total surface area, add in minimum side area ("slack")
    (2 * areas.sum) + areas.min
  }

  override protected def doSolutionB(input: String): String = {
    input.split('\n').map(calcRibbon).sum.toString
  }

  private def calcRibbon(input: String): Int = {
    val Array(l, w, h) = input.split("x").map(_.toInt)
    //Calculate perimeter of each face, find min
    val shortestFace = List(l + w, w + h, h + l).map(2 * _).min
    //Return vol + shortest face perimeter
    shortestFace + (l * w * h)
  }
}
