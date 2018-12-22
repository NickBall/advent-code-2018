package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

object Day16 extends Day(16) {

  override protected def doSolutionA(input: String): String = {
    val samples = parseInput(input)._1

    samples.map(s => {
      Device.instructions.map(i => {
        val regC = i(s.before, s.instruction.a, s.instruction.b)
        val actual = s.before.updated(s.instruction.c, regC)
        actual
      }).count(x => x == s.after)
    }).count(s => s >= 3).toString
  }

  override protected def doSolutionB(input: String): String = {
    var opcodeCandidates = (0 until 16).map(i => i -> Device.instructions).toMap

    //Pass thru samples, finding matching instructions and opcodes
    val in = parseInput(input)
    val samples = in._1
    val instructions = in._2
    println(s"Loaded ${samples.length} samples")
    samples.map(s => {
      //Figure out matching instructions
      val matches = Device.instructions.map(i => {
        val regC = i(s.before, s.instruction.a, s.instruction.b)
        val actual = s.before.updated(s.instruction.c, regC)
        i -> actual
      }).filter(x => x._2 == s.after).map(_._1).toSet
      s.instruction.opcode -> matches
    }).foreach(s => {
      val intersect = opcodeCandidates(s._1).toSet.intersect(s._2).toSeq
      opcodeCandidates = opcodeCandidates.updated(s._1, intersect)
    })

    //Pass thru the candidates, reducing by removing exclusives from lists with multiples
    var opcodes = Map.empty[Int, Device.Instruction]
    while (opcodes.size < Device.instructions.size) {
      val exclusives = opcodeCandidates.find(_._2.size == 1)
      exclusives.foreach(e => {
        opcodes += (e._1 -> e._2.head)
      })
      //Filter out finals from opcode candidates
      opcodeCandidates = opcodeCandidates.map(o => o._1 -> o._2.filterNot(v => opcodes.values.toSeq.contains(v)))
    }

    //Process instructions, get final register 0 value
    instructions.foldLeft(Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0))((reg, i) => {
      val res = opcodes(i.opcode)(reg, i.a, i.b)
      reg.updated(i.c, res)
    })(0).toString
  }

  protected def parseInput(input: String): (Seq[Sample], Seq[Instruction]) = {
    val regexBefore = """Before:\s+\[(\d), (\d), (\d), (\d)\]""".r
    val regexAfter = """After:\s+\[(\d), (\d), (\d), (\d)\]""".r
    val regexInstruction = """(\d+)\s+(\d)\s+(\d)\s+(\d)""".r

    val lines = input.split("\n")

    var samples = Seq[Sample]()
    var instructions = Seq[Instruction]()

    var before: Option[Map[Int, Int]] = None
    var instruction: Option[Instruction] = None
    lines.foreach {
      case regexBefore(b0, b1, b2, b3) => before = Some(Seq(b0.toInt, b1.toInt, b2.toInt, b3.toInt).zipWithIndex.map(x => x._2 -> x._1).toMap)
      case regexInstruction(i0, i1, i2, i3) =>
        val i = Instruction(i0.toInt, i1.toInt, i2.toInt, i3.toInt)
        //Input file is split into 2 blocks, parse instruction based on context
        if (before.isDefined) {
          instruction = Some(i)
        } else {
          instructions = instructions :+ i
        }
      case regexAfter(a0, a1, a2, a3) =>
        val after = Seq(a0.toInt, a1.toInt, a2.toInt, a3.toInt).zipWithIndex.map(x => x._2 -> x._1).toMap
        if (before.isDefined && instruction.isDefined) {
          val sample = Sample(before.get, instruction.get, after)
          samples = samples :+ sample
        }
        before = None
        instruction = None
      case _ => //NOOP
    }

    (samples, instructions)
  }
}

case class Sample(before: Map[Int, Int], instruction: Instruction, after: Map[Int, Int])

case class Instruction(opcode: Int, a: Int, b: Int, c: Int)

object Device {

  def instructions: Seq[Instruction] = Seq(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)

  trait Instruction {
    implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

    def apply(registers: Map[Int, Int], a: Int, b: Int): Int = doIt(registers, a, b)

    def doIt(registers: Map[Int, Int], a: Int, b: Int): Int
  }

  object addr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) + registers(b)
  }

  object addi extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) + b
  }

  object mulr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) * registers(b)
  }

  object muli extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) * b
  }

  object banr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) & registers(b)
  }

  object bani extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) & b
  }

  object borr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) | registers(b)
  }

  object bori extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) | b
  }

  object setr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a)
  }

  object seti extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = a
  }

  object gtir extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = a > registers(b)
  }

  object gtri extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) > b
  }

  object gtrr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) > registers(b)
  }

  object eqir extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = a == registers(b)
  }

  object eqri extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) == b
  }

  object eqrr extends Instruction {
    override def doIt(registers: Map[Int, Int], a: Int, b: Int): Int = registers(a) == registers(b)
  }

}
