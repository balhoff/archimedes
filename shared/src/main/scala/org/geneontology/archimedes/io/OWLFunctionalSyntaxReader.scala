package org.geneontology.archimedes.io

import fastparse.{Parsed, parse}
import org.geneontology.archimedes.io.OWLFunctionalSyntaxParser.{PrefixedFunctionalSyntaxParser, prefixMap}
import org.geneontology.archimedes.owl.{Axiom, ClassExpression, Ontology}

object OWLFunctionalSyntaxReader {

  def readOntology(text: String): Either[String, Ontology] = {
    parse(text, prefixMap(_)) match {
      case Parsed.Success(pm, _)          =>
        val parser = new PrefixedFunctionalSyntaxParser(pm)
        parse(text, parser.ontology(_)) match {
          case Parsed.Success(ont, _)         => Right(ont)
          case fail @ Parsed.Failure(_, _, _) => Left(fail.toString)
        }
      case fail @ Parsed.Failure(_, _, _) => Left(fail.toString)
    }
  }

  def parseClassExpression(text: String, prefixMap: Map[String, String] = Map.empty): Either[String, ClassExpression] = {
    val parser = new PrefixedFunctionalSyntaxParser(prefixMap)
    parse(text, parser.classExpression(_)) match {
      case Parsed.Success(ce, _)          => Right(ce)
      case fail @ Parsed.Failure(_, _, _) => Left(fail.toString)
    }
  }

  def parseAxiom(text: String, prefixMap: Map[String, String] = Map.empty): Either[String, Axiom] = {
    val parser = new PrefixedFunctionalSyntaxParser(prefixMap)
    parse(text, parser.axiom(_)) match {
      case Parsed.Success(axiom, _)       => Right(axiom)
      case fail @ Parsed.Failure(_, _, _) => Left(fail.toString)
    }
  }

}
