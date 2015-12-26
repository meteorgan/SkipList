package org.skiplist


class SkipListNode[K, V](val level: Int) {
    var key: K = _
    var value: V = _
    def this(key: K, value: V, level: Int) = {
        this(level)
        this.key = key
        this.value = value
    }

    val forwards = Array.fill[SkipListNode[K, V]](level){null}
    var spans = new Array[Int](level)   // 前向节点到下一个节点的距离
    var backward: SkipListNode[K, V] = null

    override def toString() = {
        s"node[$key, $value]"
    }
}

object SkipListNode {
    def main(args: Array[String]) = {
        println(new SkipListNode[Double, Int](3))
    }
}