package sBitTorrent

import scala.collection.immutable.SortedMap

class Bencode {
    def encode(value: Any): String = {
        value match {
            case i: Int => encode(i)
            case s: String => encode(s)
            case lst: List[_] => encode(lst)
            case dict: Map[_, _] => encode(dict.asInstanceOf[Map[String, Any]])
        }
    }

    private def encode(value: Int) = s"i${value}e"

    private def encode(value: String) = s"${value.length}:${value}"

    private def encode(value: List[Any]): String =
        "l" + value.map(encode(_)).mkString + "e"

    private def encode(value: Map[String, Any]): String =
        "d" + value.toList.sortBy(_._1).map(item => encode(item._1) + encode(item._2)).mkString + "e"


    def decode(value: String): Any = decodeFirst(value)._1

    private def decodeFirst(value: String): (Any, Int) = {
        value.head match {
            case 'i' =>
                val pos = value.indexOf('e')
                val v = value.substring(1, pos).toInt
                (v, pos)
            case 'l' =>
                var result = decodeFirst(value.tail)
                var pos = 0
                var lst: List[Any] = Nil
                while(result._1 != null) {
                    lst = result._1 +: lst
                    pos += result._2 + 1
                    result = decodeFirst(value.substring(pos+1))
                }
                (lst.reverse, pos+1)
            case 'd' =>
                var pos = 0
                var key = decodeFirst(value.tail)
                var map = SortedMap[String, Any]()
                while(key._1 != null) {
                    pos += key._2 + 1
                    val v = decodeFirst(value.substring(pos+1))
                    map += (key._1.asInstanceOf[String] -> v._1)
                    pos += v._2 + 1
                    key = decodeFirst(value.substring(pos+1))
                }
                (map, pos+1)
            case x if x >= '0' && x <= '9' =>
                var pos = value.indexOf(':')
                val length = value.substring(0, pos).toInt
                val v = value.substring(pos+1, pos+length+1)
                pos += length
                (v, pos)
            case 'e' => (null, 0)
        }
    }
}

object Bencode {
    def Bencode(): Bencode = {
        new Bencode()
    }

    def main(args: Array[String]) = {
        val bencode = Bencode()
        val str = "i34e5:xxxoo"
        println(bencode.decodeFirst(str))
        val str1 = "5:xxxooi34e"
        println(bencode.decodeFirst(str1))
        val str3 = "li12e3:xxxli-10ei9e1:m3:xxxee"
        println(bencode.decodeFirst(str3))
        val str4 = "d3:xxx2:oo1:mi24e3:ganli9e3:wenee"
        println(bencode.decodeFirst(str4))
    }
}