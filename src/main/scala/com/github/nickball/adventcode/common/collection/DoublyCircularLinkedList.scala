package com.github.nickball.adventcode.common.collection

import com.github.nickball.adventcode.common.collection.DoublyCircularLinkedList.{Empty, MaybeNode, Node}

import scala.reflect.ClassTag

sealed class DoublyCircularLinkedList[A] {
  private var current: MaybeNode[A] = Empty[A]()
  private var _size = 0
  private var firstNode: MaybeNode[A] = Empty[A]()

  private var bookmarks: Map[String, MaybeNode[A]] = Map.empty[String, MaybeNode[A]]

  def add(x: A): Unit = {
    current match {
      case _: Empty[A] =>
        current = Node(x)
        firstNode = current
      case _ =>
        val newPrev: Node[A] = current.asInstanceOf[Node[A]]
        val newNext: Node[A] = current.asInstanceOf[Node[A]].next.asInstanceOf[Node[A]]
        current = Node[A](x, newNext, newPrev)
        newPrev.next = current
        newNext.prev = current
    }
    _size += 1
  }

  def first: Option[A] = {
    node2Option(_first)
  }

  def size: Int = _size

  def last: Option[A] = {
    node2Option(_last)
  }

  private def _last: MaybeNode[A] = {
    _first match {
      case _: Node[A] => prev(); current
      case _ => Empty[A]
    }
  }

  private def _first: MaybeNode[A] = {
    current = firstNode
    current
  }

  def prev(): Unit = {
    current match {
      case _: Empty[A] =>
        current = current
      case _ =>
        current = current.asInstanceOf[Node[A]].prev
    }
  }

  private def node2Option(maybeNode: MaybeNode[A]): Option[A] = {
    maybeNode match {
      case f: Node[A] => Some(f.value)
      case _: Empty[A] => Option.empty
    }
  }

  def loadBookmark(label: String): Option[A] = {
    val found = bookmarks.get(label)
    found match {
      case found: Some[MaybeNode[A]] =>
        current = found.get
        get
      case _ =>
        Option.empty[A]
    }
  }

  def get: Option[A] = {
    node2Option(current)
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

        //Remove bookmark
        val removedBookmarks = bookmarks.filter(b => b._2 eq current).keys
        bookmarks --= removedBookmarks

        Option(old.asInstanceOf[Node[A]].value)
    }
  }

  def setBookmark(label: String): Option[A] = {
    bookmarks += (label -> current)
    get
  }

  override def toString: String = {
    toSeq.toString
  }

  def toSeq[A: ClassTag]: Seq[A] = {
    val starting = current
    val stuff = Array.ofDim[A](_size)

    var i = 0
    do {
      stuff(i) = current.asInstanceOf[Node[A]].value
      next()
      i += 1
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