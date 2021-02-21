package org.geneontology.archimedes.owl

import org.geneontology.archimedes.owl.OWLVocabulary._
import org.geneontology.archimedes.util.Lists.PluralList
import org.geneontology.archimedes.util.Sets.{NonEmptySet, PluralSet}

import scala.collection.immutable.ListSet

final case class IRI(id: String) extends AnnotationSubject with AnnotationValue

final case class OntologyID(iri: IRI, versionIRI: Option[IRI])

final case class Ontology(id: Option[OntologyID], imports: Set[IRI], annotations: Set[Annotation], axioms: Set[Axiom])

sealed trait OWLObject //extends HasSignature

sealed trait Entity extends OWLObject

sealed trait Individual extends OWLObject with IArg

sealed trait PropertyExpression

final case class NamedIndividual(iri: IRI) extends Individual with Entity

// PN_LOCAL
final case class AnonymousIndividual(id: String) extends Individual with AnnotationSubject with AnnotationValue

sealed trait ObjectPropertyExpression extends PropertyExpression with SubObjectPropertyExpression

final case class ObjectProperty(iri: IRI) extends ObjectPropertyExpression with Entity

final case class ObjectInverseOf(inverse: ObjectProperty) extends ObjectPropertyExpression

sealed trait PropertyRange

sealed trait ClassExpression extends OWLObject with PropertyRange

sealed trait AnonymousClassExpression extends ClassExpression

sealed trait Restriction[P <: PropertyExpression] extends AnonymousClassExpression {

  def property: P

}

sealed trait ObjectRestriction extends Restriction[ObjectPropertyExpression]

sealed trait QuantifiedRestriction[P <: PropertyExpression, F <: PropertyRange] extends Restriction[P] {

  def filler: F

}

sealed trait QuantifiedObjectRestriction extends QuantifiedRestriction[ObjectPropertyExpression, ClassExpression] with ObjectRestriction

sealed trait CardinalityRestriction[P <: PropertyExpression, F <: PropertyRange] extends QuantifiedRestriction[P, F] {

  def cardinality: Int

  //TODO
  //def isQualified

}

sealed trait ObjectCardinalityRestriction extends CardinalityRestriction[ObjectPropertyExpression, ClassExpression] with QuantifiedObjectRestriction

sealed trait HasValueRestriction[V] {

  def value: V

}

final case class Class(iri: IRI) extends ClassExpression with Entity

final case class ObjectIntersectionOf(operands: PluralSet[ClassExpression]) extends ClassExpression

final case class ObjectUnionOf(operands: PluralSet[ClassExpression]) extends ClassExpression

final case class ObjectComplementOf(complement: ClassExpression) extends ClassExpression

final case class ObjectOneOf(individuals: NonEmptySet[Individual]) extends ClassExpression

final case class ObjectSomeValuesFrom(property: ObjectPropertyExpression, filler: ClassExpression) extends QuantifiedObjectRestriction

final case class ObjectAllValuesFrom(property: ObjectPropertyExpression, filler: ClassExpression) extends QuantifiedObjectRestriction

final case class ObjectHasValue(property: ObjectPropertyExpression, value: Individual) extends ObjectRestriction with HasValueRestriction[Individual]

final case class ObjectHasSelf(property: ObjectPropertyExpression) extends ObjectRestriction

// non-negative
final case class ObjectMinCardinality(cardinality: Int, property: ObjectPropertyExpression, filler: ClassExpression = OWLThing) extends ObjectCardinalityRestriction

// non-negative
final case class ObjectMaxCardinality(cardinality: Int, property: ObjectPropertyExpression, filler: ClassExpression = OWLThing) extends ObjectCardinalityRestriction

// non-negative
final case class ObjectExactCardinality(cardinality: Int, property: ObjectPropertyExpression, filler: ClassExpression = OWLThing) extends ObjectCardinalityRestriction

sealed trait DataRestriction extends Restriction[DataProperty]

sealed trait QuantifiedDataRestriction extends QuantifiedRestriction[DataProperty, DataRange] with DataRestriction

//TODO document not using list of properties
final case class DataSomeValuesFrom(property: DataProperty, filler: DataRange) extends QuantifiedDataRestriction

//TODO document not using list of properties
final case class DataAllValuesFrom(property: DataProperty, filler: DataRange) extends QuantifiedDataRestriction

final case class DataHasValue(property: DataProperty, value: Literal) extends DataRestriction with HasValueRestriction[Literal]

final case class DataMinCardinality(cardinality: Int, property: DataProperty, filler: Option[DataRange]) extends ClassExpression

final case class DataMaxCardinality(cardinality: Int, property: DataProperty, filler: Option[DataRange]) extends ClassExpression

final case class DataExactCardinality(cardinality: Int, property: DataProperty, filler: Option[DataRange]) extends ClassExpression

final case class DataProperty(iri: IRI) extends Entity with PropertyExpression

sealed trait DataRange extends OWLObject with PropertyRange

final case class DataIntersectionOf(operands: PluralSet[DataRange]) extends DataRange

final case class DataUnionOf(operands: PluralSet[DataRange]) extends DataRange

final case class DataComplementOf(complement: DataRange) extends DataRange

final case class DataOneOf(values: NonEmptySet[Literal]) extends DataRange

final case class Facet(iri: IRI) extends OWLObject

final case class FacetRestriction(facet: Facet, value: Literal) extends OWLObject

final case class DatatypeRestriction(datatype: Datatype, facetRestrictions: NonEmptySet[FacetRestriction]) extends DataRange

final case class Datatype(iri: IRI) extends DataRange with Entity

sealed trait Literal extends AnnotationValue with DArg with OWLObject

final case class TypedLiteral(lexicalForm: String, datatype: Datatype) extends Literal

final case class PlainLiteral(lexicalForm: String, language: Option[String]) extends Literal

final case class Annotation(property: AnnotationProperty, value: AnnotationValue, annotations: Set[Annotation] = Set.empty) extends OWLObject

final case class AnnotationProperty(iri: IRI) extends Entity

sealed trait AnnotationSubject

sealed trait AnnotationValue

sealed trait Axiom extends OWLObject

final case class Declaration(entity: Entity, annotations: Set[Annotation]) extends Axiom

sealed trait AnnotationAxiom extends Axiom

final case class AnnotationAssertion(property: AnnotationProperty, subject: AnnotationSubject, value: AnnotationValue, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

final case class SubAnnotationPropertyOf(subProperty: AnnotationProperty, superProperty: AnnotationProperty, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

final case class AnnotationPropertyDomain(property: AnnotationProperty, domain: IRI, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

final case class AnnotationPropertyRange(property: AnnotationProperty, range: IRI, annotations: Set[Annotation] = Set.empty) extends AnnotationAxiom

sealed trait LogicalAxiom extends Axiom

sealed trait ClassAxiom extends LogicalAxiom

final case class SubClassOf(subClass: ClassExpression, superClass: ClassExpression, annotations: Set[Annotation] = Set.empty) extends ClassAxiom

final case class EquivalentClasses(expressions: PluralSet[ClassExpression], annotations: Set[Annotation] = Set.empty) extends ClassAxiom

final case class DisjointClasses(expressions: PluralSet[ClassExpression], annotations: Set[Annotation] = Set.empty) extends ClassAxiom

final case class DisjointUnion(namedClass: Class, expressions: PluralSet[ClassExpression], annotations: Set[Annotation] = Set.empty) extends ClassAxiom

sealed trait ObjectPropertyAxiom extends LogicalAxiom

sealed trait SubObjectPropertyExpression extends OWLObject

final case class ObjectPropertyChain(properties: PluralList[ObjectPropertyExpression]) extends SubObjectPropertyExpression

final case class SubObjectPropertyOf(subProperty: SubObjectPropertyExpression, superProperty: ObjectPropertyExpression, annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class EquivalentObjectProperties(properties: PluralSet[ObjectPropertyExpression], annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

final case class DisjointObjectProperties(properties: PluralSet[ObjectPropertyExpression], annotations: Set[Annotation] = Set.empty) extends ObjectPropertyAxiom

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

sealed trait DataPropertyAxiom extends LogicalAxiom

final case class SubDataPropertyOf(subProperty: DataProperty, superProperty: DataProperty, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class EquivalentDataProperties(properties: PluralSet[DataProperty], annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DisjointDataProperties(properties: PluralSet[DataProperty], annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DataPropertyDomain(property: DataProperty, domain: ClassExpression, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DataPropertyRange(property: DataProperty, range: DataRange, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class FunctionalDataProperty(property: DataProperty, annotations: Set[Annotation] = Set.empty) extends DataPropertyAxiom

final case class DatatypeDefinition(datatype: Datatype, datarange: DataRange, annotations: Set[Annotation] = Set.empty) extends Axiom

// ops or dps (or both) must be larger than zero...
final case class HasKey(classExpression: ClassExpression, objectProperties: Set[ObjectPropertyExpression], dataProperties: Set[DataProperty], annotations: Set[Annotation] = Set.empty) extends Axiom

sealed trait Assertion extends LogicalAxiom

final case class SameIndividual(individuals: PluralSet[Individual], annotations: Set[Annotation] = Set.empty) extends Assertion

final case class DifferentIndividuals(individuals: PluralSet[Individual], annotations: Set[Annotation] = Set.empty) extends Assertion

final case class ClassAssertion(classExpression: ClassExpression, individual: Individual, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class ObjectPropertyAssertion(property: ObjectPropertyExpression, source: Individual, target: Individual, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class NegativeObjectPropertyAssertion(property: ObjectPropertyExpression, source: Individual, target: Individual, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class DataPropertyAssertion(property: DataProperty, source: Individual, value: Literal, annotations: Set[Annotation] = Set.empty) extends Assertion

final case class NegativeDataPropertyAssertion(property: DataProperty, source: Individual, value: Literal, annotations: Set[Annotation] = Set.empty) extends Assertion

//TODO change to ListSet?
final case class DLSafeRule(body: Set[Atom], head: Set[Atom], annotations: Set[Annotation] = Set.empty) extends LogicalAxiom

sealed trait Atom extends OWLObject

sealed trait IArg extends OWLObject

sealed trait DArg extends OWLObject

final case class Variable(iri: IRI) extends IArg with DArg

final case class ClassAtom(predicate: ClassExpression, arg: IArg) extends Atom

final case class ObjectPropertyAtom(predicate: ObjectPropertyExpression, source: IArg, target: IArg) extends Atom

final case class SameIndividualAtom(first: IArg, second: IArg) extends Atom

final case class DifferentIndividualsAtom(first: IArg, second: IArg) extends Atom

final case class DataRangeAtom(predicate: DataRange, arg: DArg) extends Atom

final case class DataPropertyAtom(predicate: DataProperty, source: IArg, value: DArg) extends Atom

// one or more
final case class BuiltInAtom(iri: IRI, arguments: List[DArg]) extends Atom
