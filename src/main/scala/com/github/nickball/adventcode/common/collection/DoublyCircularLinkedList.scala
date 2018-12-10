package com.github.nickball.adventcode.common.collection

import com.github.nickball.adventcode.common.collection.DoublyCircularLinkedList.{Empty, MaybeNode, Node}

sealed class DoublyCircularLinkedList[A] {
  private var current: MaybeNode[A] = Empty[A]()
  private var _size = 0

  def add(x: A): Unit = {
    current match {
      case _: Empty[A] =>
        current = Node(x)
      case _ =>
        val newPrev: Node[A] = current.asInstanceOf[Node[A]]
        val newNext: Node[A] = current.asInstanceOf[Node[A]].next.asInstanceOf[Node[A]]
        current = Node[A](x, newNext, newPrev)
        newPrev.next = current
        newNext.prev = current
    }
    _size += 1
  }

  def get: Option[A] = {
    current match {
      case _: Empty[A] => Option.empty
      case _ => Option(current.asInstanceOf[Node[A]].value)
    }
  }

  def prev(): Unit = {
    current match {
      case _: Empty[A] =>
        current = current
      case _ =>
        current = current.asInstanceOf[Node[A]].prev
    }
  }

  def remove(): Option[A] = {
    current match {
      case _: Empty[A] => Option.empty
      case _ =>
        val old = current
        current = old.asInstanceOf[Node[A]].prev
        if (old eq current) {
          current = Empty()
        } else {
          current.asInstanceOf[Node[A]].next = old.asInstanceOf[Node[A]].next
        }
        _size -= 1
        Option(old.asInstanceOf[Node[A]].value)
    }
  }

  def size: Int = _size

  override def toString: String = {
    toSeq.toString
  }

  def toSeq: Seq[A] = {
    val starting = current
    var stuff = Seq[A]()

    do {
      stuff = stuff :+ current.asInstanceOf[Node[A]].value
      next()
    } while (current ne starting)

    stuff
  }

  def next(): Unit = {
    current match {
      case _: Empty[A] =>
        current = current
      case _ =>
        current = current.asInstanceOf[Node[A]].next
    }
  }
}

object DoublyCircularLinkedList {

  private sealed trait MaybeNode[A]

  private case class Empty[A]() extends MaybeNode[A]

  private class Node[A](var value: A, var next: MaybeNode[A], var prev: MaybeNode[A]) extends MaybeNode[A]

  private object Node {
    def apply[A](value: A): Node[A] = {
      val me = new Node[A](value, Empty[A](), Empty[A]())
      me.prev = me
      me.next = me
      me
    }

    def apply[A](value: A, next: MaybeNode[A], prev: MaybeNode[A]): Node[A] = {
      new Node[A](value, next, prev)
    }
  }

}