package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.mutable

object Day01 extends Day(1) {

  override protected def doSolutionA(input: String): String = {
    mapInput(input).sum.toString
  }

  override protected def doSolutionB(input: String): String = {
    //Create a circular continual iterator
    val it = Iterator.continually(mapInput(input)).flatten.scanLeft(0)(_ + _)

    var found = Option.empty[Int]
    val known = mutable.Set[Int]()
    while (found.isEmpty) {
      val freq = it.next()
      //Add returns false when element is already in set
      if (!known.add(freq)) {
        found = Some(freq)
      }
    }
    found.get.toString
  }

  //Split input + map into Ints
  private def mapInput(input: String): Traversable[Int] = input.split("\n").map(_.toInt)
}
