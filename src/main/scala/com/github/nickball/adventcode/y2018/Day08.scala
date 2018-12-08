package com.github.nickball.adventcode.y2019

import com.github.nickball.adventcode.Day

object Day08 extends Day(8) {

  override protected def doSolutionA(inputStr: String): String = {
    val root = input2Node(inputStr)
    sumMetadata(root.head).toString
  }

  override protected def doSolutionB(input: String): String = {
    val root = input2Node(input)
    sumNode(root.head).toString
  }

  private def sumNode(input: Node): Int = input match {
    case e if e.children.isEmpty =>
      // No children, sum the metadata
      e.metadata.map(_.value).sum
    case c => c.metadata.map(m => {
      //Get child at 1-based index
      c.children.zipWithIndex.map(_.swap).toMap.get(m.value - 1) match {
        case None => 0 //No child at index
        case x: Some[Node] => sumNode(x.get) //Sum child node
      }
    }).sum
  }

  private def input2Node(inputStr: String) = {
    var input = Vector[Int]()
    input ++= inputStr.split(" ").map(_.toInt)
    parseNode(input)
  }

  private def parseNode(input: Vector[Int]): Seq[Node] = {
    def iterate(input: => Vector[Int], acc: Seq[Node]): (Vector[Int], Seq[Node]) = {
      var mine = input
      //Grab the headers, drop from our vector
      val Array(header_children, header_metadata) = mine.take(2).toArray
      mine = mine.drop(2)

      var children = Seq[Node]()
      //Parse the children nodes
      (0 until header_children).foreach(_ => {
        val res = iterate(mine, children)
        mine = res._1
        children = res._2
      })

      //Load metadata
      val metadata = mine.take(header_metadata).map(Metadata)
      mine = mine.drop(header_metadata)
      (mine, acc :+ Node(children, metadata))
    }

    iterate(input, List.empty)._2
  }

  private def sumMetadata(input: Node): Int = {
    input.children.map(sumMetadata).sum + input.metadata.map(_.value).sum
  }

  private def flatten(input: List[Node]): List[Node] = input flatMap {
    case x if x.children.nonEmpty => x +: flatten(x.children.toList)
    case x => List(x)
  }

  case class Metadata(value: Int)

  case class Node(children: Seq[Node], metadata: Seq[Metadata])

}
