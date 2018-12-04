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
    println("Input loaded, running...")
    //TODO these times are with a cold JVM, but whatever
    println(s"A: ${time(SolutionA(in))}")
    println(s"B: ${time(SolutionB(in))}")
  }

  protected def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  protected def readInput(): String = {
    val dayStr = "%02d".format(day)
    Source.fromInputStream(getClass.getResourceAsStream(s"/input/$dayStr.txt")).getLines().mkString("\n")
  }

  PrintSolutions()
}
