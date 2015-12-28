package sBitTorrent

import org.scalatest.FlatSpec

import scala.collection.immutable.SortedMap

class BencodeTest extends FlatSpec {
    "bencode" should "decode int value" in {
        val bencode = Bencode.Bencode()
        val str1 = "i399e"
        assert(bencode.decode(str1) == 399)
        val str2 = "i0e"
        assert(bencode.decode(str2) == 0)
        val str3 = "i-19e"
        assert(bencode.decode(str3) == -19)
    }

    "bencode" should "decode string value" in {
        val bencode = Bencode.Bencode()
        val str1 = "1:e"
        assert(bencode.decode(str1) == "e")
        val str2 = "3:xf5"
        assert(bencode.decode(str2) == "xf5")
        val str3 = "11:xfwfglwgefn"
        assert(bencode.decode(str3) == "xfwfglwgefn")
        val str4 = "0:"
        assert(bencode.decode(str4) == "")
    }

    "bencode" should "decode List" in {
        val bencode = Bencode.Bencode()
        val str1 = "l4:spam4:eggse"
        assert(bencode.decode(str1) == List("spam", "eggs"))
        val str2 = "li34e5:xxxoo3:eggi-10ei43ee"
        assert(bencode.decode((str2)) == List(34, "xxxoo", "egg", -10, 43))
        val str3 = "l4:spam4:eggsli34e5:xxxoo3:eggi-10ei43eee"
        assert(bencode.decode(str3) == List("spam", "eggs", List(34, "xxxoo", "egg", -10, 43)))
        val str4 = "li54eli4e3:mmmli9ei65eeed3:gan3:wen4:xxxxl3:mmmeee"
        assert(bencode.decode(str4) == List(54, List(4, "mmm", List(9, 65)),
                                        SortedMap("gan"->"wen", "xxxx"->List("mmm"))))
    }

    "bencode" should "decode map" in {
        val bencode = Bencode.Bencode()
        val str1 = "d1:rd2:id20:mnopqrstuvwxyz123456e1:t2:aa1:y1:re"
        assert(bencode.decode(str1) == SortedMap("t"->"aa", "y"->"r", "r"-> SortedMap("id"->"mnopqrstuvwxyz123456")))
        val str2 = "d1:rd2:id20:abcdefghij01234567895:token8:aoeusnth6:valuesl6:axje.u6:idhtnmee1:t2:aa1:y1:re"
        val map2 = SortedMap("t"->"aa", "y"->"r",
                "r"->SortedMap("id"->"abcdefghij0123456789",
                               "token"->"aoeusnth",
                               "values"-> List("axje.u", "idhtnm")))
        assert(bencode.decode(str2) == map2)
    }

    "bencode" should "encode int" in {
        val bencode = Bencode.Bencode()
        val int1 = 0
        assert(bencode.encode(int1) == "i0e")
        val int2 = 34
        assert(bencode.encode(int2) == "i34e")
        val int3 = -8
        assert(bencode.encode(int3) == "i-8e")
    }

    "bencode" should "encode string" in {
        val bencode = Bencode.Bencode()
        val str1 = ""
        assert(bencode.encode(str1) == "0:")
        val str2 = "rt"
        assert(bencode.encode(str2) == "2:rt")
    }

    "bencode" should "encode list" in {
        val bencode = Bencode.Bencode()
        val list1 = List(1, 3, "xxx", 4, "mmm")
        assert(bencode.encode(list1) == "li1ei3e3:xxxi4e3:mmme")
        val list2 = List("xxx", List(4, 3, "mmm"), "haha")
        assert(bencode.encode(list2) == "l3:xxxli4ei3e3:mmme4:hahae")
    }

    "bencode" should "encode map" in {
        val bencode = Bencode.Bencode()
        val map1 = Map("mmm"->"haha", "oh"->123)
        assert(bencode.encode(map1) == "d3:mmm4:haha2:ohi123ee")
        val map2 = Map("mm"->"ha", "ha"->List(1, 2, "123"))
        assert(bencode.encode(map2) == "d2:hali1ei2e3:123e2:mm2:hae")
        val map3 = Map("gan"->List(3, List("oh", "god")), "m"->SortedMap("a"->"ha"))
        assert(bencode.encode(map3) == "d3:ganli3el2:oh3:godee1:md1:a2:haee")
    }
}
