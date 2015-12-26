package org.skiplist


class SkipListNode(val key: String, var value: Int, val level: Int) {
    val forwards = Array.fill[SkipListNode](level){null}
    var spans = new Array[Int](level)   // 前向节点到下一个节点的距离
    var backward: SkipListNode = null

    override def toString() = {
        s"node[$key, $value]"
    }
}