package org.skiplist

import scala.util.Random

class SkipList {
  import SkipList.{maxLevel, prob}

  private val random = new Random(System.currentTimeMillis())
  private var currentLevel = 1
  private var size = 0
  private val header = new SkipListNode("header", 0, maxLevel)
  private var tail = header.forwards(0)
  private var _searchTimes = 0

  def length = size

  def search(key: String): Option[SkipListNode] = {
    _searchTimes = 0
    var node = header
    for(level <- currentLevel-1 to 0 by -1) {
      while(node.forwards(level) != null && node.forwards(level).key < key) {
        node = node.forwards(level)
        _searchTimes += 1
      }
    }
    if(node.forwards(0) != null && node.forwards(0).key == key)
      Some(node.forwards(0))
    else
      None
  }

  protected[skiplist] def searchTimes = _searchTimes

  def max: Option[SkipListNode] = Option(tail)

  def min: Option[SkipListNode] = Option(header.forwards(0))

  def insert(key: String, value: Int): Unit = {
    val update = Array.fill[SkipListNode](currentLevel)(null)
    var node = header
    for(i <- currentLevel-1 to 0 by -1) {
      while(node.forwards(i) != null && node.forwards(i).key < key)
        node = node.forwards(i)
      update(i) = node
    }

    if(node.forwards(0) != null && node.forwards(0).key == key) {
      node.forwards(0).value = value
    }
    else {
      val level = randomLevel()
      val newNode = new SkipListNode(key, value, level)
      val minLevel = currentLevel.min(level)
      if(level > currentLevel) {
        for(i <- level-1 to currentLevel by -1) {
          newNode.forwards(i) = header.forwards(i)
          header.forwards(i) = newNode
        }
        currentLevel = level
      }
      for(i <- minLevel-1 to 0 by -1) {
        newNode.forwards(i) = update(i).forwards(i)
        update(i).forwards(i) = newNode
      }
      if(newNode.forwards(0) == null) {
        tail = newNode
      }

      size += 1
    }
  }

  def delete(key: String): Option[SkipListNode] = {
    val update = Array.fill[SkipListNode](currentLevel)(null)
    var node = header
    for(i <- currentLevel-1 to 0 by -1) {
      while(node.forwards(i) != null && node.forwards(i).key < key)
        node = node.forwards(i)
      update(i) = node
    }
    val target = node.forwards(0)
    if(target != null && target.key == key) {
      for(i <- target.level-1 to 0 by -1) {
        update(i).forwards(i) = target.forwards(i)
        target.forwards(i) = null
      }
      if(update(0).forwards(0) == null) {
        tail = update(0)
      }
      for(i <- target.level-1 to 0 by -1 if header.forwards(i) == null)
        currentLevel -= 1

      size -= 1
      Some(node)
    }
    else
      None
  }

  private def randomLevel(): Int = {
    var level = 1
    while((random.nextDouble() < prob) && (level < maxLevel))
      level += 1

    level
  }

  protected[skiplist] def printList(): Unit = {
    var node = header
    print(node)
    while(node.forwards(0) != null) {
      print("-> " + node.forwards(0))
      node = node.forwards(0)
    }
    println()
  }
}

object SkipList {
  val maxLevel = 32
  val prob = 0.5

  def List(): SkipList = {
    new SkipList
  }

  def main(args: Array[String]) {
    val list = List()
    list.insert("d", 12)
    list.insert("g", 14)
    list.insert("a", 51)
    list.insert("gd", 1)
    list.printList()
    println(list.max.get)
  }
}