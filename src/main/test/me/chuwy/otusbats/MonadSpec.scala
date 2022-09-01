package me.chuwy.otusbats

import me.chuwy.otusbats.Monad
import org.specs2.mutable._

class MonadSpec extends Specification {
  "This is a specification to check Monad type class behaviour".br

  "Client should be able to use summoner to package value to monad" >> {
    val optMonad: Monad[Option] = Monad[Option]
    optMonad must beAnInstanceOf[Monad[Option]]

    val listMonad: Monad[List] = Monad[List]
    listMonad must beAnInstanceOf[Monad[List]]
  }

  "Monads should implement it's functions  correctly" >> {
    "for Option" >> {
      val optMonad: Monad[Option] = Monad[Option]
      optMonad.map(Some(42))(_ * 2) must beSome(84)
      optMonad.flatMap(Some(4))(x => Some(x * 3)) must beSome(12)
      optMonad.flatMap(None: Option[Int])(x => Some(x * 3)) must beNone
      optMonad.flatten(Some(Some(42))) must beSome(42)
      optMonad.flatten(Some(None)) must beNone
    }
    "for List" >> {
      val listMonad = Monad[List]
      listMonad.map(List.range(1,4))(_ * 3) must beEqualTo(List(3, 6, 9))
      listMonad.map(List.empty[String])(x => f"$x transformed") must beEqualTo(List.empty[String])

      listMonad.flatMap(List("q", "we"))(List(_, "23")) must beEqualTo(List("q", "23", "we", "23"))
      listMonad.flatMap(List(1, 5))(_ => List(42)) must beEqualTo(List(42, 42))

      listMonad.flatten(List(List(42), List(84), List(1))) must beEqualTo(List(42, 84, 1))
    }
    "for Set" >> {
      val setMonad = Monad[Set]
      setMonad.map(Set.range(1, 4))(_ * 3) must beEqualTo(Set(3, 6, 9))
      setMonad.map(Set.empty[String])(x => f"$x transformed") must beEqualTo(Set.empty[String])

      setMonad.flatMap(Set("q", "we", "q", "we"))(Set(_, "23")) must beEqualTo(Set("q", "23", "we", "23"))
      setMonad.flatMap(Set(1, 5))(_ => Set(42)) must beEqualTo(Set(42, 42))

      setMonad.flatten(Set(Set(42), Set(84), Set(1))) must beEqualTo(Set(42, 84, 1))
    }
    "for Either" >> {
      val eitherMonad = Monad[Monad.EitherT]
      eitherMonad.map(Right(42))(_ * 2) must beRight(84)
      eitherMonad.map(Left("qwe"): Monad.EitherT[String])(x => s"Error occurred: $x") must beLeft("qwe")
    }
  }
}
