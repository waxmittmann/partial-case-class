package mwittmann.test

import mwittmann.partialcaseclasses.partial

object PartialCaseClasses {

  @partial("a", "c")
  case class SomeCaseclass(a: String, b: Option[Int], c: List[String])

  def main(args: Array[String]): Unit = {
    val c = SomeCaseclass("a", Some(1), List("foo"))

    val d = c.SomeCaseclass_derived("a", List("b", "c"))

    println(d)
  }
}
