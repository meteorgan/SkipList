package org.skiplist


class SkipListNode(val key: String, var value: Int, val level: Int) {
    val forwards = Array.fill[SkipListNode](level){null}
    var backward: SkipListNode = null
    var span = 0   // 到下一个节点的距离

    override def toString() = {
        s"node[$key, $value]"
    }
}