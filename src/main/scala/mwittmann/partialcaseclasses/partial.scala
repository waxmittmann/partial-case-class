package mwittmann.partialcaseclasses

import scala.annotation.StaticAnnotation
import scala.meta._

object Explorer {
  def explore(ns: Seq[scala.meta.Tree]): String =
    ns.map(explore).mkString("\n---\n")

  def explore(n: scala.meta.Tree): String =
    explore("")(n)

  def explore(indent: String)(n: scala.meta.Tree): String = {
    val thisContent = n.toString().replace("\n", " ")
    val thisType = n.getClass

    val remainder = n.children.map(explore(indent + "    ")).mkString("\n")

    s"$indent($thisType): $thisContent\n$remainder"
  }
}

object Partial {

  def collectParams(terms: Seq[Term.Arg]): Seq[String] = {
    println("Terms are: " + terms.map(Explorer.explore).mkString("\n"))

    val collectedTerms: Seq[Any] =
      terms.collect {
        case t: scala.meta.Lit => t.value
        case bad => throw new Exception(s"Cannot use term $bad")
      }

    println(s"Collected: $collectedTerms")

    val collectedVariableNames: Seq[String] =
      collectedTerms.collect {
        case s: String => s
        case bad => throw new Exception(s"Cannot use value $bad")
      }

    println(s"Collected variable names: ${collectedVariableNames}")
    collectedVariableNames
  }

}

class partial(args: Int) extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    this match {
      case v: scala.meta.Stat => println(s"Got:\n${Explorer.explore(v)}")
    }

    val q"new partial(..$terms)" = this
    val paramNames = Partial.collectParams(terms).toSet

    val q"case class $tName (..$params) extends $template" = defn

    val derivedParams = params.filter(p => paramNames.contains(p.name.value))

    val newName = tName.copy(tName.value + "_derived")

    q"""case class $tName (..$params) {
        case class $newName (..$derivedParams)
      }
     """
  }
}
