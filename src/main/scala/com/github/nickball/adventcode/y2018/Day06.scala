package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.mutable
import scala.util.matching.Regex

object Day06 extends Day(6){
 override protected def doSolutionA(input: String) : String = {
  val coords = input2coordinates(input)
  val xMax = coords.map(_._1).max
  val yMax = coords.map(_._2).max
  println(s"Using grid size ($xMax, $yMax)")

  val taxiDist = mutable.Map[(Int,Int), (Int,Int)]()

  (0 to xMax).iterator.foreach(x => {
   (0 to yMax).iterator.foreach(y => {
    //Calculate taxicab metric for each coord at x,y and sort by distance
    val distances = coords.map(p => p -> (math.abs(x - p._1) + math.abs(y - p._2))).sortBy(_._2)
    //Add shortest distance only if head and 2nd distances are not equal
    if (distances(0)._2 < distances(1)._2) {
     taxiDist.update((x,y), distances.head._1)
    }
   })
  })
  println(s"Grid locations count: ${taxiDist.size}")

  //Strip out infinite areas - ones that manhattan distances are on the grid min/max
  val excluded = taxiDist.filter(t => (Set(0,xMax).contains(t._1._1) || Set(yMax,0).contains(t._1._2)) ).values.toSet
  println(s"Filtered out infinite coordinates: ${excluded.toString}")


  //Group by coordinate, filter out excluded coords, determine counts, sort by largest distance
  val sorted = taxiDist.groupBy(_._2).filterNot(t => excluded.contains(t._1)).mapValues(_.size).toSeq.sortBy(_._2).reverse
  println("--------------------------------------------------------------")
  sorted.foreach(x => println(s"${x._1} : ${x._2}"))
  println("--------------------------------------------------------------")
  println(s"Winner: ${sorted.head._1} with distance ${sorted.head._2}")
  sorted.head._2.toString
 }

 private def input2coordinates(input: String): Array[(Int, Int)] = {
  val pattern: Regex = """(\d+), (\d+)""".r
  val coords = input.split("\n").map(l => l match {
   case pattern(x, y) => (x.toInt, y.toInt)
  })
  coords
 }

 override protected def doSolutionB(input: String): String = {

  val coords = input2coordinates(input)
  val xMax = coords.map(_._1).max
  val yMax = coords.map(_._2).max
  println(s"Using grid size ($xMax, $yMax)")

  //FIXME ugly hack to determine input file vs unit test max distance, because
  // the test "framework" currently doesn't support additional parameters
  val max_distance = (xMax * yMax) match {
   case x if x > 10000 => 10000
   case _ => 32
  }

  val taxiDist = mutable.Map[(Int,Int), Int]()

  //Iterate over grid x,y
  (0 to xMax).iterator.foreach(x => {
   (0 to yMax).iterator.foreach(y => {
    //Calculate taxicab metric for each coord at (x,y) and sum
    val distance = coords.map(p => (math.abs(x - p._1) + math.abs(y - p._2))).sum
    taxiDist.update((x,y), distance)
   })
  })

  taxiDist.filter(_._2 < max_distance).size.toString
 }
}
