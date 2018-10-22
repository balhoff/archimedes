package org.geneontology.archimedes.owl

final case class IRI(id: String) extends AnnotationSubject with AnnotationValue

final case class OntologyID(iri: IRI, versionIRI: Option[IRI])

final case class Ontology(id: Option[OntologyID], imports: Set[IRI], annotations: Set[Annotation], axioms: Set[Axiom])

sealed trait Entity

sealed trait Individual

final case class NamedIndividual(iri: IRI) extends Individual with Entity

// PN_LOCAL
final case class AnonymousIndividual(id: String) extends Individual with AnnotationSubject with AnnotationValue

sealed trait ObjectPropertyExpression extends SubObjectPropertyExpression

final case class ObjectProperty(iri: IRI) extends ObjectPropertyExpression with Entity

final case class ObjectInverseOf(inverse: ObjectProperty) extends ObjectPropertyExpression

sealed trait ClassExpression

final case class Class(iri: IRI) extends ClassExpression with Entity

// set of at least 2
final case class ObjectIntersectionOf(operands: Set[ClassExpression]) extends ClassExpression

// set of at least 2
final case class ObjectUnionOf(operands: Set[ClassExpression]) extends ClassExpression

final case class ObjectComplementOf(complement: ClassExpression) extends ClassExpression

// set of at least 1
final case class ObjectOneOf(individuals: Set[Individual]) extends ClassExpression

final case class ObjectSomeValuesFrom(property: ObjectPropertyExpression, filler: ClassExpression) extends ClassExpression

final case class ObjectAllValuesFrom(property: ObjectPropertyExpression, filler: ClassExpression) extends ClassExpression

final case class ObjectHasValue(property: ObjectPropertyExpression, value: Individual) extends ClassExpression

final case class ObjectHasSelf(property: ObjectPropertyExpression) extends ClassExpression

// non-negative
final case class ObjectMinCardinality(cardinality: Int, property: ObjectPropertyExpression, filler: Option[ClassExpression]) extends ClassExpression

// non-negative
final case class ObjectMaxCardinality(cardinality: Int, property: ObjectPropertyExpression, filler: Option[ClassExpression]) extends ClassExpression

// non-negative
final case class ObjectExactCardinality(cardinality: Int, property: ObjectPropertyExpression, filler: Option[ClassExpression]) extends ClassExpression

// size of list must match arity of data range
final case class DataSomeValuesFrom(properties: List[DataProperty], filler: DataRange) extends ClassExpression

// size of list must match arity of data range
final case class DataAllValuesFrom(properties: List[DataProperty], filler: DataRange) extends ClassExpression

final case class DataHasValue(property: DataProperty, value: Literal) extends ClassExpression

final case class DataMinCardinality(cardinality: Int, property: DataProperty, filler: Option[DataRange]) extends ClassExpression

final case class DataMaxCardinality(cardinality: Int, property: DataProperty, filler: Option[DataRange]) extends ClassExpression

final case class DataExactCardinality(cardinality: Int, property: DataProperty, filler: Option[DataRange]) extends ClassExpression

final case class DataProperty(iri: IRI) extends Entity

sealed trait DataRange

// set of at least 2
final case class DataIntersectionOf(operands: Set[DataRange]) extends DataRange

final case class DataUnionOf(operands: Set[DataRange]) extends DataRange

final case class DataComplementOf(complement: DataRange) extends DataRange

// set of at least 1
final case class DataOneOf(values: Set[Literal]) extends DataRange

final case class Facet(iri: IRI)

final case class FacetRestriction(facet: Facet, value: Literal)

// set of at least 1
final case class DatatypeRestriction(datatype: Datatype, facetRestrictions: Set[FacetRestriction]) extends DataRange

final case class Datatype(iri: IRI) extends DataRange with Entity

sealed trait Literal extends AnnotationValue

final case class TypedLiteral(lexicalForm: String, datatype: Datatype) extends Literal

final case class PlainLiteral(lexicalForm: String, language: Option[String]) extends Literal

final case class Annotation(property: AnnotationProperty, value: AnnotationValue, annotations: Set[Annotation] = Set.empty)

final case class AnnotationProperty(iri: IRI) extends Entity

sealed trait AnnotationSubject

sealed trait AnnotationValue

sealed trait Axiom

final case class Declaration(entity: Entity, annotations: Set[Annotation]) extends Axiom

sealed trait AnnotationAxiom extends Axiom

final case class AnnotationAssertion(property: AnnotationProperty, subject: AnnotationSubject, value: AnnotationValue, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

final case class SubAnnotationPropertyOf(subProperty: AnnotationProperty, superProperty: AnnotationProperty, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

final case class AnnotationPropertyDomain(property: AnnotationProperty, domain: IRI, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

final case class AnnotationPropertyRange(property: AnnotationProperty, range: IRI, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

sealed trait ClassAxiom extends Axiom

final case class SubClassOf(subClass: ClassExpression, superClass: ClassExpression, annotations: Set[Annotation] = Set.empty) extends ClassAxiom

// set of at least 2
final case class EquivalentClasses(expressions: Set[ClassExpression], annotations: Set[Annotation] = Set.empty) extends ClassAxiom

// set of at least 2
final case class DisjointClasses(expressions: Set[ClassExpression], annotations: Set[Annotation] = Set.empty) extends ClassAxiom

// set of at least 2
final case class DisjointUnion(namedClass: Class, expressions: Set[ClassExpression], annotations: Set[Annotation] = Set.empty) extends ClassAxiom

sealed trait ObjectPropertyAxiom extends Axiom

sealed trait SubObjectPropertyExpression

// list of at least 2
final case class ObjectPropertyChain(properties: List[ObjectPropertyExpression]) extends SubObjectPropertyExpression

final case class SubObjectPropertyOf(subProperty: SubObjectPropertyExpression, superProperty: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

// set of at least 2
final case class EquivalentObjectProperties(properties: Set[ObjectPropertyExpression], annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

// set of at least 2
final case class DisjointObjectProperties(properties: Set[ObjectPropertyExpression], annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class ObjectPropertyDomain(property: ObjectPropertyExpression, domain: ClassExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class ObjectPropertyRange(property: ObjectPropertyExpression, range: ClassExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class InverseObjectProperties(first: ObjectPropertyExpression, second: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class FunctionalObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class InverseFunctionalObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class ReflexiveObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class IrreflexiveObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class SymmetricObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class AsymmetricObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class TransitiveObjectProperty(property: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

sealed trait DataPropertyAxiom extends Axiom

final case class SubDataPropertyOf(subProperty: DataProperty, superProperty: DataProperty, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

// set of at least 2
final case class EquivalentDataProperties(properties: Set[DataProperty], annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DisjointDataProperties(properties: Set[DataProperty], annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DataPropertyDomain(property: DataProperty, domain: ClassExpression, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DataPropertyRange(property: DataProperty, range: DataRange, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class FunctionalDataProperty(property: DataProperty, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DatatypeDefinition(datatype: Datatype, datarange: DataRange, annotations: Set[Annotation] = Set.empty) extends Axiom

final case class HasKey(classExpression: ClassExpression, objectProperties: Set[ObjectPropertyExpression], dataProperties: Set[DataProperty], annotations: Set[Annotation] = Set.empty) extends Axiom

sealed trait Assertion extends Axiom

// set of at least 2
final case class SameIndividual(individuals: Set[Individual], annotations: Set[Annotation] = Set.empty) extends Assertion

// set of at least 2
final case class DifferentIndividuals(individuals: Set[Individual], annotations: Set[Annotation] = Set.empty) extends Assertion

final case class ClassAssertion(classExpression: ClassExpression, individual: Individual, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class ObjectPropertyAssertion(property: ObjectPropertyExpression, source: Individual, target: Individual, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class NegativeObjectPropertyAssertion(property: ObjectPropertyExpression, source: Individual, target: Individual, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class DataPropertyAssertion(property: DataProperty, source: Individual, value: Literal, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class NegativeDataPropertyAssertion(property: DataProperty, source: Individual, value: Literal, annotations: Set[Annotation] = Set.empty) extends Assertion
