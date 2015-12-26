package org.skiplist

import org.scalatest.FlatSpec

import scala.util.Random

class SkipListSpec extends FlatSpec {
    val random = new Random(System.currentTimeMillis())
    val maxValue = 100000
    val values = IndexedSeq.range(0, maxValue)
    val chars = IndexedSeq.range('a','z')

    def randomValue: Int = {
        val pos = random.nextInt(maxValue)
        values(pos)
    }

    def randomString: String = {
        val length = random.nextInt(20)
        val str = StringBuilder.newBuilder
        for(i <- 0 to length) {
            val pos = random.nextInt(chars.length)
            str += chars(pos)
        }

        str.toString()
    }

    "insert " should "has length of insert nodes" in {
        val list = SkipList.List()
        var set = Set[String]()
        val size = 1000
        for(i <- 0 until size) {
            val key = randomString
            val value = randomValue
            list.insert(key, value)
            set += key
        }
        assert(list.length == set.size)
    }

    "delete" should "has right length after delete" in {
        val list = SkipList.List()
        val size = 3333
        val deleteSize = 10

        var map = Map[String, Int]()
        for(i <- 0 until size) {
            val key = randomString
            val value = randomValue
            map += (key -> value)
            list.insert(key, value)
        }
        val keys = map.keys.toSeq
        for(i <- 0 until deleteSize)
            list.delete(keys(i))

        assert(list.length == map.size - deleteSize)
    }

    "search" should "find the existed elements" in {
        val list = SkipList.List()
        val size = 1200000
        val deleteSize = 20000
        var map = Map[String, Int]()
        for(i <- 0 until size) {
            val key = randomString
            val value = randomValue
            map += (key -> value)
            list.insert(key, value)
        }
        val keys = map.keys.toSeq
        for(i <- 0 until deleteSize) {
            val key = keys(i)
            val value = list.search(key).get.value
            assert(value == map(key))

            list.delete(key)
            assert(list.search(key) == None)
        }
    }

    "min/max" should "return the min/max item" in {
        val list = SkipList.List()
        val size = 89000
        var map = Map[String, Int]()
        for(i <- 0 until size) {
            val key = randomString
            val value = randomValue
            map += (key -> value)
            list.insert(key, value)
        }

        assert(list.min.get.key == map.keys.min)
        assert(list.max.get.key == map.keys.max)
    }
    it should "return None if list is empty" in {
        val list = SkipList.List()
        assert(list.min == None)
        assert(list.max == None)
    }

    def statsSearch {
        val total = 1100000
        var testcase = Map[String, Int]()
        for(i <- 1 to total) {
            testcase += (randomString -> randomValue)
        }
        val keys = testcase.keys.toSeq
        val existsNumber = keys.length - keys.length/10

        val lst = SkipList.List()
        for(i <- 0 until existsNumber) {
            val key = keys(i)
            lst.insert(key, testcase(key))
        }

        var existSearchTimes = List[Int]()
        var notExistSearchTimes = List[Int]()
        for(i <- 0 until existsNumber) {
            val key = keys(i)
            val value = lst.search(key).get.value
            assert(value == testcase(key))
            existSearchTimes = lst.searchTimes :: existSearchTimes
        }
        for(i <- existsNumber until keys.length) {
            val key = keys(i)
            val v = lst.search(key)
            assert(v == None)
            notExistSearchTimes = lst.searchTimes :: notExistSearchTimes
        }


        val sortedExists = existSearchTimes.sorted
        val existMiddle = sortedExists(sortedExists.length/2)
        val existAverage = sortedExists.sum / sortedExists.length
        val ninetyPercent = sortedExists(sortedExists.length - sortedExists.length/10)
        println(s"exsit search, max: ${existSearchTimes.max}, min: ${existSearchTimes.min}, " +
            s"average: ${existAverage}, middle: ${existMiddle}, ninety: ${ninetyPercent}")

        val sortedNotExists = notExistSearchTimes.sorted
        val notExistMiddle = sortedNotExists(sortedNotExists.length/2)
        val notExistAverage = sortedNotExists.sum / sortedNotExists.length
        val ninetyPercent1 = sortedNotExists(sortedNotExists.length - sortedNotExists.length/10)
        println(s"not exsit search, max: ${notExistSearchTimes.max}, min: ${notExistSearchTimes.min}, " +
            s"average: ${notExistAverage}, middle: ${notExistMiddle}, ninety: ${ninetyPercent1}")
    }
}