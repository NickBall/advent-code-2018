package com.github.nickball.adventcode.common.collection

import org.scalatest.FunSuite

class DoublyCircularLinkedListTest extends FunSuite {

  test("next should advance current list element") {
    val list = new DoublyCircularLinkedList[Int]
    list.add(0)
    list.add(1)
    list.add(2)

    assert(list.size == 3)
    assert(list.get.get == 2)
    list.next() //Wrap around
    assert(list.get.get == 0)
    list.next()
    assert(list.get.get == 1)
    list.next()
    assert(list.get.get == 2)
  }

  test("next should NOOP for empty list") {
    val list = new DoublyCircularLinkedList[Int]
    list.next()
    assert(list.size == 0)
  }

  test("prev should go to previous list element") {
    val list = new DoublyCircularLinkedList[Int]
    list.add(0)
    list.add(1)
    list.add(2)

    assert(list.size == 3)
    assert(list.get.get == 2)
    list.prev()
    assert(list.get.get == 1)
    list.prev()
    assert(list.get.get == 0)
    list.prev() //Wrap around
    assert(list.get.get == 2)
  }

  test("add should add elements after current element") {
    val list = new DoublyCircularLinkedList[Int]
    assert(list.size == 0)
    assert(list.get.isEmpty)

    list.add(42)
    assert(list.size == 1)
    assert(list.get.get == 42)

    list.add(0)
    assert(list.size == 2)
    assert(list.get.get == 0)
    assert(list.toSeq == List(0, 42))
    list.first
    assert(list.toSeq == List(42, 0))

    list.first
    list.add(1)
    assert(list.size == 3)
    assert(list.get.get == 1)
    list.first
    assert(list.toSeq == List(42, 1, 0))
  }

  test("last should go to 'last' element in list") {
    val list = new DoublyCircularLinkedList[Int]
    list.add(0)
    list.add(1)
    list.add(2)
    list.add(3)

    list.first
    assert(list.get.get == 0)

    list.next()
    assert(list.get.get == 1)

    list.last
    assert(list.get.get == 3)
  }

  test("size should return size of list") {
    val list = new DoublyCircularLinkedList[Int]
    assert(list.size == 0)
    list.add(0)
    assert(list.size == 1)
    list.add(1)
    assert(list.size == 2)
    list.add(2)
    assert(list.size == 3)

    list.remove()
    assert(list.size == 2)
    list.remove()
    assert(list.size == 1)
    list.remove()
    assert(list.size == 0)
  }

  test("first should go to 'first' element in list") {
    val list = new DoublyCircularLinkedList[Int]
    list.add(0)
    list.add(1)
    list.add(2)
    list.add(3)

    assert(list.get.get == 3)
    list.prev()
    assert(list.get.get == 2)

    list.first
    assert(list.get.get == 0)
    list.prev()
    assert(list.get.get == 3)
  }

}
