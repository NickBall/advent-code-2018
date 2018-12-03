package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.mutable

object Day03 extends Day(3) {

  override protected def doSolutionA(input: String): String = {
    val lines = input.split("\n")
    //Sparse matrix tracking duplicate claims
    val claims = new mutable.HashMap[Coord, mutable.Set[Int]] with mutable.MultiMap[Coord, Int]
    lines.map(l => Claim.create(l)).foreach(c => {
      for (x <- c.start.x until c.start.x + c.size.x) {
        for (y <- c.start.y until c.start.y + c.size.y) {
          val i = Coord.create(x, y)
          //Add to multimap
          claims.addBinding(i, c.num)
        }
      }
    })

    //Filter for coords with multiple claim numbers
    claims.values.count(_.size > 1).toString
  }

  override protected def doSolutionB(input: String): String = {
    //this one is similar to A, but we have more tracking to do in the loop

    val lines = input.split("\n")
    val claims = new mutable.HashMap[Coord, mutable.Set[Int]] with mutable.MultiMap[Coord, Int]
    //track claim numbers with no conflict yet
    val candidates = new mutable.HashSet[Int]

    lines.map(l => Claim.create(l)).foreach(c => {
      //track if this current claim has conflict
      var conflict = false
      for (x <- c.start.x until c.start.x + c.size.x) {
        for (y <- c.start.y until c.start.y + c.size.y) {
          val i = Coord.create(x, y)
          //get any existing claims
          val existingClaims = claims.get(i)
          if (existingClaims.nonEmpty) {
            //have conflict, remove any existing candidates that share this claim
            conflict = true
            candidates --= existingClaims.get
          }
          claims.addBinding(i, c.num)
        }
      }
      if (!conflict) {
        //add claim to candidate list, since no conflict found
        candidates += c.num
      }
    })

    //expecting only 1 candidate
    if (candidates.size != 1) {
      throw new IllegalStateException("Should only be 1 candidate! Found: " + candidates)
    }
    candidates.head.toString
  }

  case class Claim(num: Int, start: Coord, size: Coord) {
    def apply(num: Int, start: Coord, size: Coord): Claim = new Claim(num, start, size)
  }

  case class Coord private(x: Int, y: Int) {
    def apply(x: Int, y: Int): Coord = new Coord(x, y)
  }

  object Claim {
    def create(input: String): Claim = {
      val pattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
      input match {
        case pattern(num, x, y, dimX, dimY) => new Claim(num.toInt, Coord.create(x, y), Coord.create(dimX, dimY))
      }
    }
  }

  object Coord {
    def create(x: String, y: String): Coord = {
      create(x.toInt, y.toInt)
    }

    def create(x: Int, y: Int): Coord = {
      apply(x, y)
    }
  }

}
