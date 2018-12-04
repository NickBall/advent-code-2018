package com.github.nickball.adventcode.common.collection

import scala.collection.mutable

trait ListMultiMap[A, B] extends mutable.Map[A, mutable.MutableList[B]] {

  def addBinding(key: A, value: B): this.type = {
    get(key) match {
      case None =>
        val list = makeList
        list += value
        this (key) = list
      case Some(list) =>
        list += value
    }
    this
  }

  protected def makeList: mutable.MutableList[B] = new mutable.MutableList[B]
}
