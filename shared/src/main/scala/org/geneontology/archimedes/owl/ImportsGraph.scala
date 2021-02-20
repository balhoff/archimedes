package org.geneontology.archimedes.owl

final case class ImportsGraph(root: Ontology, imports: Map[IRI, Ontology]) {

  def axioms: Set[Axiom] = ???

  def directImports: Set[Ontology] = root.imports.map(imports)

}
