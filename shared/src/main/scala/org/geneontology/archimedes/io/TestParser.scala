package org.geneontology.archimedes.io

import fastparse._
import org.geneontology.archimedes.io.OWLFunctionalSyntaxParser._

import scala.io.Source

object TestParser extends App {

  def time[T](f: => T): T = {
    val start = System.currentTimeMillis()
    val res = f
    val stop = System.currentTimeMillis()
    val elapsed = stop - start
    println(s"Time: $elapsed ms")
    res
  }

  val text = "\"hello world\"@en"
  println(text)
  println(parse(text, stringLiteral(_)))


  println(parse("anon", PN_LOCAL(_)))

  val ont1 = Source.fromFile("uberon.ofn", "UTF-8").getLines() //.mkString
  val ont2 = Source.fromFile("uberon.ofn", "UTF-8").getLines() //.mkString
  val ont3 = Source.fromFile("uberon.ofn", "UTF-8").getLines().mkString("\n")
  val ont4 = Source.fromFile("uberon.ofn", "UTF-8").getLines().map(l => s"$l\n")

  val myIt = new Iterator[String] {

    override def hasNext: Boolean = ont2.hasNext

    var nl = false

    def next(): String = {
      if (nl) {
        nl = false
        "\n"
      } else {
        nl = true
        ont2.next()
      }
    }

  }

  val Parsed.Success(pm, _) = parse(ont1, prefixMap(_))

  val parser = new PrefixedFunctionalSyntaxParser(pm)

  val Parsed.Success(myOnt, _) = time(parse(myIt, parser.ontology(_)))

  val Parsed.Success(ont3p, _) = time(parse(ont3, parser.ontology(_)))

  val Parsed.Success(ont4p, _) = time(parse(ont4, parser.ontology(_)))

  println(myOnt == ont3p)
  println(myOnt.id == ont3p.id)
  println(myOnt.annotations == ont3p.annotations)
  println(myOnt.imports == ont3p.imports)
  println(myOnt.axioms -- ont3p.axioms)
  println(ont3p.axioms -- myOnt.axioms)

}
