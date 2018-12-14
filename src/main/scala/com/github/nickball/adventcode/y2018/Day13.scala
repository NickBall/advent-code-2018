package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

object Day13 extends Day(13) {

  override protected def doSolutionB(input: String) :String = {
    var carts = locateCarts(input)
    val emptyMap = clearMap(input)

    var map = input
    while(carts.size > 1) {
      val resp = moveCarts(map, emptyMap, carts)
      map = resp._1
      carts = resp._2

      println(s"Tick $i")
      map.split("\n").foreach(l => println(l))
      println()
    }
    val c = carts.head.loc
    s"${c.x},${c.y}"
  }
  override protected def doSolutionA(input: String): String = {
    var carts = locateCarts(input)
    val emptyMap = clearMap(input)

    var map = input
    var collision = Option.empty[Point]
    var i = 0
    while (collision.isEmpty) {
      val resp = moveCarts(map, emptyMap, carts)
      map = resp._1
      carts = resp._2

      collision = hasCollision(map)
      i += 1
    }

    val c = collision.get
    s"${c.x},${c.y}"
  }

  def clearMap(input: String): String = {
    input.replaceAll("[<>]", "-").replaceAll("[v^]", "|")
  }

  def moveCarts(input: String, empty: String, carts: Seq[Cart]): (String, Seq[Cart]) = {
    var newMap = input.split("\n")
    val cleanMap = empty.split("\n")
    var newCarts = Seq.empty[Cart]
    var collision = false
    carts.takeWhile(_ => !collision).foreach(cart => {
      val oldLoc = cart.loc
      //Get new location
      val newLoc = cart.direction match {
        case Direction.Up => Point(oldLoc.x, oldLoc.y - 1)
        case Direction.Down => Point(oldLoc.x, oldLoc.y + 1)
        case Direction.Left => Point(oldLoc.x - 1, oldLoc.y)
        case Direction.Right => Point(oldLoc.x + 1, oldLoc.y)
      }
      //Get tile value at new location
      val newTile = newMap(newLoc.y).charAt(newLoc.x)
      var newDirection = cart.direction
      var nextTurn = cart.nextTurn
      val newVal = newTile match {
        //Collision!
        case 'v' => 'X'
        case '^' => 'X'
        case '<' => 'X'
        case '>' => 'X'
        //Just moving straight
        case '-' => cart.direction.tileVal
        case '|' => cart.direction.tileVal
        //Turns!
        case '+' => cart.nextTurn match {
          case Direction.Left =>
            nextTurn = Direction.Straight
            newDirection = cart.direction match {
              case Direction.Left => Direction.Down
              case Direction.Right => Direction.Up
              case Direction.Up => Direction.Left
              case Direction.Down => Direction.Right
            }
            newDirection.tileVal
          case Direction.Straight =>
            nextTurn = Direction.Right
            cart.direction.tileVal
          case Direction.Right =>
            nextTurn = Direction.Left
            newDirection = cart.direction match {
              case Direction.Left => Direction.Up
              case Direction.Right => Direction.Down
              case Direction.Up => Direction.Right
              case Direction.Down => Direction.Left
            }
            newDirection.tileVal
        }
        case '/' =>
          newDirection = cart.direction match {
            case Direction.Left => Direction.Down
            case Direction.Right => Direction.Up
            case Direction.Up => Direction.Right
            case Direction.Down => Direction.Left
          }
          newDirection.tileVal
        case '\\' =>
          newDirection = cart.direction match {
            case Direction.Left => Direction.Up
            case Direction.Right => Direction.Down
            case Direction.Up => Direction.Left
            case Direction.Down => Direction.Right
          }
          newDirection.tileVal
      }

      //Draw over old tile
      val updatedOld = newMap(oldLoc.y).updated(oldLoc.x, cleanMap(oldLoc.y).charAt(oldLoc.x))
      newMap = newMap.updated(oldLoc.y, updatedOld)

      //Replace new tile loc with new value
      val updatedNew = newMap(newLoc.y).updated(newLoc.x, newVal)
      newMap = newMap.updated(newLoc.y, updatedNew)

      val newCart = Cart(newLoc, newDirection, nextTurn)
      newCarts = newCarts :+ newCart
      collision = newVal == 'X'
    })

    (newMap.mkString("\n"), newCarts)
  }

  def hasCollision(input: String): Option[Point] = {
    input.contains("X") match {
      case false => None
      case true =>
        val lines = input.split("\n")
        (0 to lines.length).collectFirst {
          case y if lines(y).contains("X") => Point(lines(y).indexOf('X'), y)
        }
    }
  }

  def locateCarts(input: String): Seq[Cart] = {
    var carts = Seq.empty[Cart]
    val lines = input.split("\n")
    (0 until lines.length).foreach(y => {
      val line = lines(y)
      (0 until line.length).foreach(x => {
        val cart = line(x) match {
          case '^' => Some(Cart(Point(x, y), Direction.Up, Direction.Left))
          case 'v' => Some(Cart(Point(x, y), Direction.Down, Direction.Left))
          case '<' => Some(Cart(Point(x, y), Direction.Left, Direction.Left))
          case '>' => Some(Cart(Point(x, y), Direction.Right, Direction.Left))
          case _ => None
        }
        if (cart.isDefined) {
          carts = carts :+ cart.get
        }
      })
    })
    carts
  }



  case class Cart(loc: Point, direction: Direction.EnumVal, nextTurn: Direction.EnumVal)

  case class Point(x: Int, y: Int)

  class Direction

  object Direction {

    sealed abstract class EnumVal(val tileVal: Char)

    case object Up extends EnumVal('^')

    case object Down extends EnumVal('v')

    case object Left extends EnumVal('<')

    case object Right extends EnumVal('>')

    case object Straight extends EnumVal('-')

    val directions: Seq[Direction.EnumVal] = Seq(Up, Down, Left, Right, Straight)
  }

}
