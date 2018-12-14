package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.language.postfixOps

object Day13 extends Day(13) {

  override protected def doSolutionB(input: String): String = {
    var carts = locateCarts(input)
    val emptyMap = clearMap(input)

    var map = input
    while (carts.size > 1) {
      val resp = moveCarts(map, emptyMap, carts)
      map = resp._1
      carts = resp._2
    }
    val c = carts.head.loc
    s"${c.x},${c.y}"
  }

  override protected def doSolutionA(input: String): String = {
    var carts = locateCarts(input)
    val emptyMap = clearMap(input)

    var map = input
    var collisions = Seq.empty[Cart]
    var i = 0
    while (collisions.isEmpty) {
      val resp = moveCarts(map, emptyMap, carts)
      map = resp._1
      carts = resp._2
      collisions = resp._3

      i += 1
    }

    val c = collisions.minBy(c => c).loc
    s"${c.x},${c.y}"
  }

  def clearMap(input: String): String = {
    input.replaceAll("[<>]", "-").replaceAll("[v^]", "|")
  }

  def moveCarts(input: String, empty: String, carts: Seq[Cart]): (String, Seq[Cart], Seq[Cart]) = {
    var newMap = input.split("\n")
    val cleanMap = empty.split("\n")
    var newCarts = Seq.empty[Cart]
    var collisions = Seq.empty[Cart]
    carts.sortBy(c => c).iterator.filterNot(c => collisions.contains(c)).foreach(cart => {
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

      //Add cart to collisions if needed
      val newCart = Cart(newLoc, newDirection, nextTurn)
      if (newVal == 'X') {
        collisions = collisions :+ newCart
      } else {
        newCarts = newCarts :+ newCart
      }

      //Draw over old tile
      val updatedOld = newMap(oldLoc.y).updated(oldLoc.x, cleanMap(oldLoc.y).charAt(oldLoc.x))
      newMap = newMap.updated(oldLoc.y, updatedOld)

      //Replace new tile loc with new value or clear collision
      var updatedNew = newMap(newLoc.y).updated(newLoc.x, newVal)
      if (newVal == 'X') {
        updatedNew = newMap(newLoc.y).updated(newLoc.x, cleanMap(oldLoc.y).charAt(oldLoc.x))
        //Get other carts at location
        collisions = collisions ++: carts.filter(c => c.loc == newLoc)
        collisions = collisions ++: newCarts.filter(c => c.loc == newLoc)
        newCarts = newCarts.filterNot(c => c.loc == newLoc)
      }
      newMap = newMap.updated(newLoc.y, updatedNew)
    })

    (newMap.mkString("\n"), newCarts, collisions)
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


  case class Cart(loc: Point, direction: Direction.EnumVal, nextTurn: Direction.EnumVal) extends Ordered[Cart] {
    override def compare(that: Cart): Int = this.loc compare that.loc
  }

  case class Point(x: Int, y: Int) extends Ordered[Point] {

    import scala.math.Ordered.orderingToOrdered

    override def compare(that: Point): Int = (this.y, this.x) compare(that.y, that.x)
  }

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
