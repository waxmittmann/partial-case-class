package mwittmann.test

import mwittmann.partialcaseclasses.{PartialDefn, partial}

object PartialCaseClasses {

  //@partial("a", "c")
  @partial(
    PartialDefn("DerivedA", List("a", "b")),
    PartialDefn("DerivedB", List("a", "c"))
  )
  case class SomeCaseclass(a: String, b: Option[Int], c: List[String]) {
    case class C1(x: Int)
    case class C2(x: Int)
  }

  def main(args: Array[String]): Unit = {
    val c = SomeCaseclass("a", Some(1), List("foo"))

    val d = c.DerivedA("a", Some(32))
    val e = c.DerivedB("a", List("Hello", "World"))

    println(d)
    println(e)
  }
}
