package org.skiplist

import scala.util.Random

class SkipList[K <% Ordered[K], V] {
    import SkipList.{maxLevel, prob}

    private val random = new Random(System.currentTimeMillis())
    private var currentLevel = 1
    private var size = 0
    private val header = new SkipListNode[K, V](maxLevel)
    private var tail = header.forwards(0)
    private var _searchTimes = 0

    def length = size

    def search(key: K): Option[SkipListNode[K, V]] = {
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

    def max: Option[SkipListNode[K, V]] = Option(tail)

    def min: Option[SkipListNode[K, V]] = Option(header.forwards(0))

    def apply(index: Int): SkipListNode[K, V] = {
        if(index < 0 || index >= size)
            throw new IndexOutOfBoundsException(s"$index")
        var node = header
        var _index = -1
        for(i <- currentLevel-1 to 0 by -1) {
            while(node.forwards(i) != null && _index + node.spans(i) < index) {
                _index += node.spans(i)
                node = node.forwards(i)
            }
        }
        node.forwards(0)
    }

    def insert(key: K, value: V): Unit = {
        val update = Array.fill[SkipListNode[K, V]](currentLevel)(null)
        val spans = new Array[Int](currentLevel+1)
        var node = header
        for(i <- currentLevel-1 to 0 by -1) {
            while(node.forwards(i) != null && node.forwards(i).key < key) {
                spans(i+1) += node.spans(i)
                node = node.forwards(i)
            }
            update(i) = node
        }
        for(i <- 1 to currentLevel)
            spans(i) += spans(i-1)

        if(node.forwards(0) != null && node.forwards(0).key == key) {
            node.forwards(0).value = value
        }
        else {
            val level = randomLevel()
            val newNode = new SkipListNode[K, V](key, value, level)
            val minLevel = currentLevel.min(level)
            if(level > currentLevel) {
                for(i <- level-1 to currentLevel by -1) {
                    newNode.forwards(i) = header.forwards(i)
                    header.forwards(i) = newNode
                    header.spans(i) = spans(currentLevel) + 1
                }
                currentLevel = level
            }
            else {
                for(i <- currentLevel - 1 to minLevel by -1)
                    update(i).spans(i) += 1
            }
            for(i <- minLevel-1 to 0 by -1) {
                newNode.spans(i) = update(i).spans(i) - spans(i)
                update(i).spans(i) = spans(i) + 1

                newNode.forwards(i) = update(i).forwards(i)
                if(i == 0 && update(0).forwards(0) != null)
                    update(0).forwards(0).backward = newNode
                update(i).forwards(i) = newNode
            }
            newNode.backward = update(0)
            if(newNode.forwards(0) == null) {
                tail = newNode
            }

            size += 1
        }
    }

    def delete(key: K): Option[SkipListNode[K, V]] = {
        val update = Array.fill[SkipListNode[K, V]](currentLevel)(null)
        var node = header
        for(i <- currentLevel-1 to 0 by -1) {
            while(node.forwards(i) != null && node.forwards(i).key < key) {
                node = node.forwards(i)
            }
            update(i) = node
        }
        val target = node.forwards(0)
        if(target != null && target.key == key) {
            for(i <- target.level until currentLevel if update(i).forwards != null)
                update(i).spans(i) -= 1
            for(i <- target.level-1 to 0 by -1) {
                update(i).forwards(i) = target.forwards(i)
                update(i).spans(i) += target.spans(i) - 1
                if(i == 0 && target.forwards(0) != null)
                    target.forwards(0) = update(0)
                target.forwards(i) = null
            }
            target.backward = null
            if(update(0).forwards(0) == null) {
                tail = update(0)
            }
            for(i <- target.level-1 to 0 by -1 if header.forwards(i) == null) {
                currentLevel -= 1
            }

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
            print(" -> " + node.forwards(0))
            node = node.forwards(0)
        }
        println()
    }

    protected[skiplist] def printReverse(): Unit = {
        var node = tail
        print(node)
        while(node.backward != null) {
            print(" -> " + node.backward)
            node = node.backward
        }
        println()
    }
}

object SkipList {
    val maxLevel = 32
    val prob = 0.5

    def List[K <% Ordered[K], V](): SkipList[K, V] = {
        new SkipList[K, V]
    }

    def main(args: Array[String]) {
        val list = List[String, Int]()
        list.insert("d", 12)
        list.insert("g", 14)
        list.insert("a", 51)
        list.insert("gd", 1)
        list.printList()
        list.printReverse()
        println(list(0))
        println(list(1))
        println(list(2))
        println(list(3))
    }
}