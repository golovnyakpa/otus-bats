package me.chuwy.otusbats

import me.chuwy.otusbats.Show._
import org.specs2.mutable._


class ShowSpec extends Specification {
  "This is a specification to check Show type class behaviour".br

  "Implemented instances should be shown correctly" >> {
    "for boolean" >> {
      true.show must beEqualTo("true")
      false.show must beEqualTo("false")
    }
    "for int" >> {
      42.show must beEqualTo("42")
      0.show must beEqualTo("0")
      (-14231).show must beEqualTo("-14231")
    }
    "for string" >> {
      "qwerty".show must beEqualTo("qwerty")
      "Monads everywhere".show must beEqualTo("Monads everywhere")
    }
    "list" >> {
      val intList: List[Int] = List.range(0, 5)
      intList.show must beEqualTo("0 1 2 3 4")
      List(true, false).show must beEqualTo("true false")
    }
  }

  "Client should be able to call mkString_" >> {
    "for List[String]" >> {
      val lst: List[String] = List("a", "b", "c")
      lst.mkString_[String]("[", "]", ",") must equalTo("[a,b,c]")
    }
    "for List[Int]" >> {
      val lst = List.range(0, 5)
      lst.mkString_[Int]("{", "}", "|") must equalTo("{0|1|2|3|4}")
    }
    "for List[Boolean]" >> {
      List(false, true).mkString_[Boolean]("(", ")", "::") must equalTo("(false::true)")
    }
  }

  "Helper constructors should compile success" >> {
    Show.fromJvm[Long] must beAnInstanceOf[Show[Long]]
    Show.fromJvm[Short] must beAnInstanceOf[Show[Short]]
    Show.fromJvm[Boolean] must beAnInstanceOf[Show[Boolean]]
    Show.fromJvm[Char] must beAnInstanceOf[Show[Char]]

    Show.fromFunction[Char](x => x.toString) must beAnInstanceOf[Show[Char]]
    Show.fromFunction[Long](x => s"The Answer to the Ultimate Question of Life: $x").show(42L) must
      equalTo("The Answer to the Ultimate Question of Life: 42")

    val loudCharShow = Show.fromFunction[Char](x => s"${x.toUpper}!")
    loudCharShow.show('a') must equalTo("A!")
  }

}
