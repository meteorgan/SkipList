package org.skiplist


class SkipListNode(val key: String, var value: Int, val level: Int) {
  val forwards = Array.fill[SkipListNode](level){null}

  override def toString() = {
    s"node[$key, $value]"
  }
}