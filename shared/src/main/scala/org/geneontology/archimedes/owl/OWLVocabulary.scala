package org.geneontology.archimedes.owl

object OWLVocabulary {

  private val owlNS = "http://www.w3.org/2002/07/owl#"

  val OWLThing: Class = Class(IRI(s"${owlNS}Thing"))

  val OWLNothing: Class = Class(IRI(s"${owlNS}Nothing"))

}
