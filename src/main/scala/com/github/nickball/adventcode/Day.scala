package com.github.nickball.adventcode

import scala.io.Source

abstract class Day(day: Int) extends App {

  protected def doSolutionA(input: String): String

  protected def doSolutionB(input: String): String

  def SolutionA(): String = {
    SolutionA(readInput())
  }

  def SolutionA(input: String): String = {
    doSolutionA(input)
  }

  def SolutionB(): String = {
    SolutionB(readInput())
  }

  def SolutionB(input: String): String = {
    doSolutionB(input)
  }

  def PrintSolutions(): Unit = {
    val in = readInput()
    println(s"A: ${SolutionA(in)}")
    println(s"B: ${SolutionB(in)}")
  }

  protected def readInput(): String = {
    val dayStr = "%02d".format(day)
    Source.fromInputStream(getClass.getResourceAsStream(s"/input/$dayStr.txt")).getLines().mkString("\n")
  }

  PrintSolutions()
}
