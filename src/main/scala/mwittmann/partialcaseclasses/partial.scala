package mwittmann.partialcaseclasses

import scala.annotation.StaticAnnotation
import scala.collection.immutable
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

object PartialDefnO {

  def collectParams(annotationStat: Stat): Seq[(String, Seq[String])] = {
    val q"new partial(..$terms)" = annotationStat

    val partials: Seq[(String, Seq[Term.Arg])] = terms.map {
      case q"PartialDefn($name, List(..$terms))" =>
        name match {
          case n: scala.meta.Lit if n.value.isInstanceOf[String] => (n.value.asInstanceOf[String], terms)
          case n: scala.meta.Lit => throw new Exception(s"Cannot use type ${n.getClass()} as name of case class. Must string!")
          case bad => throw new Exception (s"Failed to match to literal: ${Explorer.explore(bad)}")
        }

      case bad => throw new Exception(s"Don't know how to use $bad\n${Explorer.explore(bad)}")
    }

    partials.map(v =>
      (
        v._1,
        v._2.collect {
          case s: Lit if s.value.isInstanceOf[String] => s.value.asInstanceOf[String]
          case s: Lit => throw new Exception(s"Expected String, got ${s.value}")
          case s => throw new Exception(s"Expected Lit, got $s")
        }
      )
    )
  }
}

case class PartialDefn(name: String, variables: List[String])

class partial(args: List[PartialDefn]) extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val paramNames: Seq[(String, Seq[String])] = PartialDefnO.collectParams(this)

    val q"case class $tName (..$params) extends $template" = defn

    val derivedClasses = paramNames.map { case (name, vars) =>
      val varsSet = vars.toSet
      val derivedParams = params.filter(p => varsSet.contains(p.name.value))
      val newName = Type.Name(name)
      q"case class $newName (..$derivedParams) extends $template"
    }

    val derivedClasses2: immutable.Seq[Stat] = derivedClasses.map(_.asInstanceOf[Stat]).to[immutable.Seq]

    q"""case class $tName (..$params) {
        ..$derivedClasses2
      }
     """
  }
}
