package org.geneontology.archimedes.io

import fastparse.ScriptWhitespace._
import fastparse._
import org.geneontology.archimedes.owl._

object OWLFunctionalSyntaxReader {

  case class PrefixDeclaration(prefix: String, expansion: IRI)

  def PN_CHARS_U[_: P]: P[Unit] = P(PN_CHARS_BASE | "_")

  def PN_CHARS[_: P]: P[Unit] = P(PN_CHARS_U | "-" | CharIn("0-9") | "\u00B7" | CharIn("\u0300-\u036F") | CharIn("\u203F-\u2040"))

  def PN_CHARS_BASE[_: P]: P[Unit] = P(CharIn("A-Z", "a-z", "\u00C0-\u00D6", "\u00D8-\u00F6", "\u00F8-\u02FF", "\u0370-\u037D", "\u037F-\u1FFF", "\u200C-\u200D", "\u2070-\u218F", "\u2C00-\u2FEF", "\u3001-\uD7FF", "\uF900-\uFDCF", "\uFDF0-\uFFFD")) // , "\u10000-\uEFFFF"))

  def PN_PREFIX[_: P]: P[Unit] = {
    import NoWhitespace._
    P(PN_CHARS_BASE ~ (PN_CHARS | ("." ~ PN_CHARS)).rep)
  }

  def PNAME_NS[_: P]: P[Unit] = {
    import NoWhitespace._
    P(PN_PREFIX.? ~ ":")
  }

  def PN_LOCAL[_: P]: P[Unit] = {
    import NoWhitespace._
    P((PN_CHARS_U | CharIn("0-9")) ~ (PN_CHARS | ("." ~ PN_CHARS)).rep)
  }

  def PNAME_LN[_: P]: P[Unit] = {
    import NoWhitespace._
    P(PNAME_NS ~ PN_LOCAL)
  }

  //FIXME check characters
  // '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
  def fullIRI[_: P]: P[IRI] = {
    import NoWhitespace._
    P(("<" ~ CharPred(c => !c.isWhitespace && (!(c == '<')) && (!(c == '>'))).rep(1).! ~ ">").map(IRI))
  }

  def anonymousIndividual[_: P]: P[AnonymousIndividual] = {
    import NoWhitespace._
    P(("_:" ~ PN_LOCAL.!).map(AnonymousIndividual))
  }

  def prefixDeclaration[_: P]: P[PrefixDeclaration] = {
    import ScriptWhitespace._
    P("Prefix" ~ "(" ~/ PNAME_NS.! ~ "=" ~ fullIRI ~ ")").map {
      case (prefix, expansion) => PrefixDeclaration(prefix, expansion)
    }
  }

  def prefixMap[_: P]: P[Map[String, String]] = {
    import ScriptWhitespace._
    P(Start ~ prefixDeclaration.rep ~ "Ontology" ~ "(").map(pds =>
      pds.map(pd => pd.prefix -> pd.expansion.id).toMap)
  }

  def abbreviatedIRI[_: P](prefixMap: Map[String, String]): P[IRI] = {
    import NoWhitespace._
    P((PNAME_NS.!.filter(prefixMap.contains) ~ PN_LOCAL.!).map {
      case (prefix, localName) => IRI(s"${prefixMap(prefix)}$localName")
    })
  }

  private def stringChars(c: Char) = c != '\"' && c != '\\'

  private def escape[_: P] = P("\\" ~ CharIn("\\\"\\\\"))

  private def strChars[_: P] = P(CharsWhile(stringChars))

  def quotedString[_: P]: P[String] = P("\"" ~/ (strChars | escape).rep.! ~ "\"")

  //FIXME use correct CharIn
  def languageTag[_: P]: P[String] = {
    import NoWhitespace._
    P("@" ~ CharIn("a-z\\-").rep.!)
  }

  def stringLiteral[_: P]: P[PlainLiteral] = {
    import NoWhitespace._
    P((quotedString ~ languageTag.?).map {
      case (text, language) => PlainLiteral(text, language)
    })
  }

  def nonNegNumber[_: P]: P[Int] = {
    import NoWhitespace._
    P(CharIn("0-9").rep(1).!.map(_.toInt))
  }

  class PrefixedFunctionalSyntaxParser(prefixes: Map[String, String]) {

    def iri[_: P]: P[IRI] = P(fullIRI | abbreviatedIRI(prefixes))

    def directlyImportsDocuments[_: P]: P[Seq[IRI]] = P(("Import" ~ "(" ~ iri ~ ")").rep)

    def ontology[_: P]: P[Ontology] = P((Start ~ prefixDeclaration.rep ~ "Ontology" ~ "(" ~/
      (iri ~ iri.?).? ~ directlyImportsDocuments ~ annotations ~ axioms ~ ")" ~ End).map {
      case (_, ontID, imps, anns, axs) =>
        val ontologyID = ontID.map(id => OntologyID(id._1, id._2))
        Ontology(ontologyID, imps.toSet, anns.toSet, axs.toSet)
    })

    def annotation[_: P]: P[Annotation] = P("Annotation" ~ "(" ~/ annotations ~ iri ~ annotationValue ~ ")").map {
      case (anns, prop, value) => Annotation(AnnotationProperty(prop), value, anns.toSet)
    }

    def annotationValue[_: P]: P[AnnotationValue] = P(anonymousIndividual | iri | literal)

    def annotationSubject[_: P]: P[AnnotationSubject] = P(anonymousIndividual | iri)

    def typedLiteral[_: P]: P[TypedLiteral] = {
      import NoWhitespace._
      P((quotedString ~ "^^" ~ iri).map {
        case (text, datatype) => TypedLiteral(text, Datatype(datatype))
      })
    }

    def literal[_: P]: P[Literal] = P(NoCut(typedLiteral) | stringLiteral)

    def annotations[_: P]: P[Seq[Annotation]] = P(annotation.rep)

    def axioms[_: P]: P[Seq[Axiom]] = P((declaration | classAxiom | objectPropertyAxiom | dataPropertyAxiom |
      datatypeDefinition | hasKey | assertion | annotationAxiom).rep)

    def declaration[_: P]: P[Declaration] = P(("Declaration" ~ "(" ~/ annotations ~ StringIn(
      "Class", "Datatype", "ObjectProperty", "DataProperty", "AnnotationProperty", "NamedIndividual").! ~
      "(" ~/ iri ~ ")" ~ ")").map {
      case (anns, kind, id) => kind match {
        case "Class"              => Declaration(Class(id), anns.toSet)
        case "Datatype"           => Declaration(Datatype(id), anns.toSet)
        case "ObjectProperty"     => Declaration(ObjectProperty(id), anns.toSet)
        case "DataProperty"       => Declaration(DataProperty(id), anns.toSet)
        case "AnnotationProperty" => Declaration(AnnotationProperty(id), anns.toSet)
        case "NamedIndividual"    => Declaration(NamedIndividual(id), anns.toSet)
      }
    })

    def classAxiom[_: P]: P[ClassAxiom] = P(subClassOf | equivalentClasses | disjointClasses | disjointUnion)

    def subClassOf[_: P]: P[SubClassOf] = P(("SubClassOf" ~ "(" ~/ annotations ~ classExpression ~ classExpression ~ ")").map {
      case (anns, subExpression, superExpression) => SubClassOf(subExpression, superExpression, anns.toSet)
    })

    def equivalentClasses[_: P]: P[EquivalentClasses] = P(("EquivalentClasses" ~ "(" ~/ annotations ~ classExpression.rep(2) ~ ")").map {
      case (anns, expressions) => EquivalentClasses(expressions.toSet, anns.toSet)
    })

    def disjointClasses[_: P]: P[DisjointClasses] = P(("DisjointClasses" ~ "(" ~/ annotations ~ classExpression.rep(2) ~ ")").map {
      case (anns, expressions) => DisjointClasses(expressions.toSet, anns.toSet)
    })

    def disjointUnion[_: P]: P[DisjointUnion] = P(("DisjointUnion" ~ "(" ~/ annotations ~ iri ~ classExpression.rep(2) ~ ")").map {
      case (anns, cls, expressions) => DisjointUnion(Class(cls), expressions.toSet, anns.toSet)
    })

    def objectPropertyAxiom[_: P]: P[ObjectPropertyAxiom] = P(subObjectPropertyOf | equivalentObjectProperties |
      disjointObjectProperties | inverseObjectProperties | objectPropertyDomain | objectPropertyRange |
      functionalObjectProperty | inverseFunctionalObjectProperty | reflexiveObjectProperty | irreflexiveObjectProperty |
      symmetricObjectProperty | asymmetricObjectProperty | transitiveObjectProperty)

    def objectPropertyExpression[_: P]: P[ObjectPropertyExpression] = P(iri.map(ObjectProperty) | inverseObjectProperty)

    def inverseObjectProperty[_: P]: P[ObjectInverseOf] = P(("ObjectInverseOf" ~ "(" ~/ iri ~ ")").map(i => ObjectInverseOf(ObjectProperty(i))))

    def propertyExpressionChain[_: P]: P[ObjectPropertyChain] = P(("ObjectPropertyChain" ~ "(" ~/ objectPropertyExpression.rep(2) ~ ")").map {
      opes => ObjectPropertyChain(opes.toList)
    })

    def subObjectPropertyOf[_: P]: P[SubObjectPropertyOf] = P(("SubObjectPropertyOf" ~ "(" ~/ annotations ~
      (objectPropertyExpression | propertyExpressionChain) ~ objectPropertyExpression ~ ")").map {
      case (anns, subProp, superProp) => SubObjectPropertyOf(subProp, superProp, anns.toSet)
    })

    def equivalentObjectProperties[_: P]: P[EquivalentObjectProperties] = P(("EquivalentObjectProperties" ~ "(" ~/ annotations ~ objectPropertyExpression.rep(2) ~ ")").map {
      case (anns, properties) => EquivalentObjectProperties(properties.toSet, anns.toSet)
    })

    def disjointObjectProperties[_: P]: P[DisjointObjectProperties] = P(("DisjointObjectProperties" ~ "(" ~/ annotations ~ objectPropertyExpression.rep(2) ~ ")").map {
      case (anns, properties) => DisjointObjectProperties(properties.toSet, anns.toSet)
    })

    def inverseObjectProperties[_: P]: P[InverseObjectProperties] = P(("InverseObjectProperties" ~ "(" ~/ annotations ~ objectPropertyExpression ~ objectPropertyExpression ~ ")").map {
      case (anns, left, right) => InverseObjectProperties(left, right, anns.toSet)
    })

    def objectPropertyDomain[_: P]: P[ObjectPropertyDomain] = P(("ObjectPropertyDomain" ~ "(" ~/ annotations ~ objectPropertyExpression ~ classExpression ~ ")").map {
      case (anns, prop, cls) => ObjectPropertyDomain(prop, cls, anns.toSet)
    })

    def objectPropertyRange[_: P]: P[ObjectPropertyRange] = P(("ObjectPropertyRange" ~ "(" ~/ annotations ~ objectPropertyExpression ~ classExpression ~ ")").map {
      case (anns, prop, cls) => ObjectPropertyRange(prop, cls, anns.toSet)
    })

    def functionalObjectProperty[_: P]: P[FunctionalObjectProperty] = P(("FunctionalObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => FunctionalObjectProperty(prop, anns.toSet)
    })

    def inverseFunctionalObjectProperty[_: P]: P[InverseFunctionalObjectProperty] = P(("InverseFunctionalObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => InverseFunctionalObjectProperty(prop, anns.toSet)
    })

    def reflexiveObjectProperty[_: P]: P[ReflexiveObjectProperty] = P(("ReflexiveObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => ReflexiveObjectProperty(prop, anns.toSet)
    })

    def irreflexiveObjectProperty[_: P]: P[IrreflexiveObjectProperty] = P(("IrreflexiveObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => IrreflexiveObjectProperty(prop, anns.toSet)
    })

    def symmetricObjectProperty[_: P]: P[SymmetricObjectProperty] = P(("SymmetricObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => SymmetricObjectProperty(prop, anns.toSet)
    })

    def asymmetricObjectProperty[_: P]: P[AsymmetricObjectProperty] = P(("AsymmetricObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => AsymmetricObjectProperty(prop, anns.toSet)
    })

    def transitiveObjectProperty[_: P]: P[TransitiveObjectProperty] = P(("TransitiveObjectProperty" ~ "(" ~/ annotations ~ objectPropertyExpression ~ ")").map {
      case (anns, prop) => TransitiveObjectProperty(prop, anns.toSet)
    })

    def dataPropertyAxiom[_: P]: P[DataPropertyAxiom] = P(subDataPropertyOf | equivalentDataProperties | disjointDataProperties |
      dataPropertyDomain | dataPropertyRange | functionalDataProperty)

    def subDataPropertyOf[_: P]: P[SubDataPropertyOf] = P(("SubDataPropertyOf" ~ "(" ~/ annotations ~ iri ~ iri ~ ")").map {
      case (anns, subProp, superProp) => SubDataPropertyOf(DataProperty(subProp), DataProperty(superProp), anns.toSet)
    })

    def equivalentDataProperties[_: P]: P[EquivalentDataProperties] = P(("EquivalentDataProperties" ~ "(" ~/ annotations ~ iri.rep(2) ~ ")").map {
      case (anns, dps) => EquivalentDataProperties(dps.map(DataProperty).toSet, anns.toSet)
    })

    def disjointDataProperties[_: P]: P[DisjointDataProperties] = P(("DisjointDataProperties" ~ "(" ~/ annotations ~ iri.rep(2) ~ ")").map {
      case (anns, dps) => DisjointDataProperties(dps.map(DataProperty).toSet, anns.toSet)
    })

    def dataPropertyDomain[_: P]: P[DataPropertyDomain] = P(("DataPropertyDomain" ~ "(" ~/ annotations ~ iri ~ classExpression ~ ")").map {
      case (anns, dp, cls) => DataPropertyDomain(DataProperty(dp), cls, anns.toSet)
    })

    def dataPropertyRange[_: P]: P[DataPropertyRange] = P(("DataPropertyRange" ~ "(" ~/ annotations ~ iri ~ dataRange ~ ")").map {
      case (anns, dp, range) => DataPropertyRange(DataProperty(dp), range, anns.toSet)
    })

    def functionalDataProperty[_: P]: P[FunctionalDataProperty] = P(("FunctionalDataProperty" ~ "(" ~/ annotations ~ iri ~ ")").map {
      case (anns, dp) => FunctionalDataProperty(DataProperty(dp), anns.toSet)
    })

    def datatypeDefinition[_: P]: P[DatatypeDefinition] = P(("DatatypeDefinition" ~ "(" ~/ annotations ~ iri ~ dataRange ~ ")").map {
      case (anns, dt, dr) => DatatypeDefinition(Datatype(dt), dr, anns.toSet)
    })

    def dataRange[_: P]: P[DataRange] = P(iri.map(Datatype) | dataIntersectionOf | dataUnionOf | dataComplementOf | dataOneOf | datatypeRestriction)

    def dataIntersectionOf[_: P]: P[DataIntersectionOf] = P(("DataIntersectionOf" ~ "(" ~/ dataRange.rep(2) ~ ")").map {
      drs => DataIntersectionOf(drs.toSet)
    })

    def dataUnionOf[_: P]: P[DataUnionOf] = P(("DataUnionOf" ~ "(" ~/ dataRange.rep(2) ~ ")").map {
      drs => DataUnionOf(drs.toSet)
    })

    def dataComplementOf[_: P]: P[DataComplementOf] = P(("DataComplementOf" ~ "(" ~/ dataRange ~ ")").map(DataComplementOf))

    def dataOneOf[_: P]: P[DataOneOf] = P(("DataOneOf" ~ "(" ~/ literal.rep(1) ~ ")").map(ls => DataOneOf(ls.toSet)))

    def datatypeRestriction[_: P]: P[DatatypeRestriction] = P(("DatatypeRestriction" ~/ "(" ~ iri ~ (iri ~ literal).rep(1) ~ ")").map {
      case (dt, facetsValues) =>
        val facets = facetsValues.map { case (facet, value) => FacetRestriction(Facet(facet), value) }.toSet
        DatatypeRestriction(Datatype(dt), facets)
    })

    def hasKey[_: P]: P[HasKey] = P(("HasKey" ~ "(" ~ annotations ~ classExpression ~ "(" ~/ objectPropertyExpression.rep ~ ")" ~ "(" ~ iri.rep ~ ")" ~ ")").map {
      case (anns, cls, ops, dps) => HasKey(cls, ops.toSet, dps.map(DataProperty).toSet, anns.toSet)
    })

    def assertion[_: P]: P[Assertion] = P(sameIndividual | differentIndividuals | classAssertion | objectPropertyAssertion |
      negativeObjectPropertyAssertion | dataPropertyAssertion | negativeDataPropertyAssertion)

    def sameIndividual[_: P]: P[SameIndividual] = P(("SameIndividual" ~ "(" ~/ annotations ~ individual.rep(2) ~ ")").map {
      case (anns, inds) => SameIndividual(inds.toSet, anns.toSet)
    })

    def differentIndividuals[_: P]: P[DifferentIndividuals] = P(("DifferentIndividuals" ~ "(" ~/ annotations ~ individual.rep(2) ~ ")").map {
      case (anns, inds) => DifferentIndividuals(inds.toSet, anns.toSet)
    })

    def classAssertion[_: P]: P[ClassAssertion] = P(("ClassAssertion" ~ "(" ~/ annotations ~ classExpression ~ individual ~ ")").map {
      case (anns, cls, ind) => ClassAssertion(cls, ind, anns.toSet)
    })

    def objectPropertyAssertion[_: P]: P[ObjectPropertyAssertion] = P(("ObjectPropertyAssertion" ~ "(" ~/ annotations ~ objectPropertyExpression ~ individual ~ individual ~ ")").map {
      case (anns, prop, source, target) => ObjectPropertyAssertion(prop, source, target, anns.toSet)
    })

    def negativeObjectPropertyAssertion[_: P]: P[NegativeObjectPropertyAssertion] = P(("NegativeObjectPropertyAssertion" ~ "(" ~/ annotations ~ objectPropertyExpression ~ individual ~ individual ~ ")").map {
      case (anns, prop, source, target) => NegativeObjectPropertyAssertion(prop, source, target, anns.toSet)
    })

    def dataPropertyAssertion[_: P]: P[DataPropertyAssertion] = P(("DataPropertyAssertion" ~ "(" ~/ annotations ~ iri ~ individual ~ literal ~ ")").map {
      case (anns, prop, source, value) => DataPropertyAssertion(DataProperty(prop), source, value, anns.toSet)
    })

    def negativeDataPropertyAssertion[_: P]: P[NegativeDataPropertyAssertion] = P(("NegativeDataPropertyAssertion" ~ "(" ~/ annotations ~ iri ~ individual ~ literal ~ ")").map {
      case (anns, prop, source, value) => NegativeDataPropertyAssertion(DataProperty(prop), source, value, anns.toSet)
    })

    def annotationAxiom[_: P]: P[AnnotationAxiom] = P(annotationAssertion | subAnnotationPropertyOf | annotationPropertyDomain | annotationPropertyRange)

    def annotationAssertion[_: P]: P[AnnotationAssertion] = P(("AnnotationAssertion" ~ "(" ~/ annotations ~ iri ~ annotationSubject ~ annotationValue ~ ")").map {
      case (anns, ap, subject, value) => AnnotationAssertion(AnnotationProperty(ap), subject, value, anns.toSet)
    })

    def subAnnotationPropertyOf[_: P]: P[SubAnnotationPropertyOf] = P(("SubAnnotationPropertyOf" ~ "(" ~/ annotations ~ iri ~ iri ~ ")").map {
      case (anns, subProp, superProp) => SubAnnotationPropertyOf(AnnotationProperty(subProp), AnnotationProperty(superProp), anns.toSet)
    })

    def annotationPropertyDomain[_: P]: P[AnnotationPropertyDomain] = P(("AnnotationPropertyDomain" ~ "(" ~/ annotations ~ iri ~ iri ~ ")").map {
      case (anns, prop, domain) => AnnotationPropertyDomain(AnnotationProperty(prop), domain, anns.toSet)
    })

    def annotationPropertyRange[_: P]: P[AnnotationPropertyRange] = P(("AnnotationPropertyRange" ~ "(" ~/ annotations ~ iri ~ iri ~ ")").map {
      case (anns, prop, range) => AnnotationPropertyRange(AnnotationProperty(prop), range, anns.toSet)
    })

    def classExpression[_: P]: P[ClassExpression] = P(iri.map(Class) | objectIntersectionOf | objectUnionOf | objectComplementOf | objectOneOf |
      objectSomeValuesFrom | objectAllValuesFrom | objectHasValue | objectHasSelf | objectMinCardinality | objectMaxCardinality |
      objectExactCardinality | dataSomeValuesFrom | dataAllValuesFrom | dataHasValue | dataMinCardinality | dataMaxCardinality | dataExactCardinality)

    def objectIntersectionOf[_: P]: P[ObjectIntersectionOf] = P(("ObjectIntersectionOf" ~ "(" ~/ classExpression.rep(2) ~ ")")
      .map(expressions => ObjectIntersectionOf(expressions.toSet)))

    def objectUnionOf[_: P]: P[ObjectUnionOf] = P(("ObjectUnionOf" ~ "(" ~/ classExpression.rep(2) ~ ")")
      .map(expressions => ObjectUnionOf(expressions.toSet)))

    def objectComplementOf[_: P]: P[ObjectComplementOf] = P(("ObjectComplementOf" ~ "(" ~/ classExpression ~ ")").map(ObjectComplementOf))

    def objectOneOf[_: P]: P[ObjectOneOf] = P(("ObjectOneOf" ~ "(" ~/ individual.rep(1) ~ ")").map(inds => ObjectOneOf(inds.toSet)))

    def objectSomeValuesFrom[_: P]: P[ObjectSomeValuesFrom] = P(("ObjectSomeValuesFrom" ~/ "(" ~ objectPropertyExpression ~ classExpression ~ ")").map {
      case (prop, cls) => ObjectSomeValuesFrom(prop, cls)
    })

    def objectAllValuesFrom[_: P]: P[ObjectAllValuesFrom] = P(("ObjectAllValuesFrom" ~/ "(" ~ objectPropertyExpression ~ classExpression ~ ")").map {
      case (prop, cls) => ObjectAllValuesFrom(prop, cls)
    })

    def objectHasValue[_: P]: P[ObjectHasValue] = P(("ObjectHasValue" ~/ "(" ~ objectPropertyExpression ~ individual ~ ")").map {
      case (prop, ind) => ObjectHasValue(prop, ind)
    })

    def objectHasSelf[_: P]: P[ObjectHasSelf] = P(("ObjectHasSelf" ~ "(" ~/ objectPropertyExpression ~ ")").map(ObjectHasSelf))

    def objectMinCardinality[_: P]: P[ObjectMinCardinality] = P(("ObjectMinCardinality" ~/ "(" ~ nonNegNumber ~ objectPropertyExpression ~ classExpression.? ~ ")").map {
      case (card, prop, filler) => ObjectMinCardinality(card, prop, filler)
    })

    def objectMaxCardinality[_: P]: P[ObjectMaxCardinality] = P(("ObjectMaxCardinality" ~/ "(" ~ nonNegNumber ~ objectPropertyExpression ~ classExpression.? ~ ")").map {
      case (card, prop, filler) => ObjectMaxCardinality(card, prop, filler)
    })

    def objectExactCardinality[_: P]: P[ObjectExactCardinality] = P(("ObjectExactCardinality" ~/ "(" ~ nonNegNumber ~ objectPropertyExpression ~ classExpression.? ~ ")").map {
      case (card, prop, filler) => ObjectExactCardinality(card, prop, filler)
    })

    def dataSomeValuesFrom[_: P]: P[DataSomeValuesFrom] = P(("DataSomeValuesFrom" ~ "(" ~/ iri.rep(1) ~ dataRange ~ ")").map {
      case (props, filler) => DataSomeValuesFrom(props.map(DataProperty).toList, filler)
    })

    def dataAllValuesFrom[_: P]: P[DataAllValuesFrom] = P(("DataAllValuesFrom" ~ "(" ~/ iri.rep(1) ~ dataRange ~ ")").map {
      case (props, filler) => DataAllValuesFrom(props.map(DataProperty).toList, filler)
    })

    def dataHasValue[_: P]: P[DataHasValue] = P(("DataHasValue" ~ "(" ~/ iri ~ literal ~ ")").map {
      case (prop, value) => DataHasValue(DataProperty(prop), value)
    })

    def dataMinCardinality[_: P]: P[DataMinCardinality] = P(("DataMinCardinality" ~ "(" ~/ nonNegNumber ~ iri ~ dataRange.? ~ ")").map {
      case (card, prop, filler) => DataMinCardinality(card, DataProperty(prop), filler)
    })

    def dataMaxCardinality[_: P]: P[DataMaxCardinality] = P(("DataMaxCardinality" ~ "(" ~/ nonNegNumber ~ iri ~ dataRange.? ~ ")").map {
      case (card, prop, filler) => DataMaxCardinality(card, DataProperty(prop), filler)
    })

    def dataExactCardinality[_: P]: P[DataExactCardinality] = P(("DataExactCardinality" ~ "(" ~/ nonNegNumber ~ iri ~ dataRange.? ~ ")").map {
      case (card, prop, filler) => DataExactCardinality(card, DataProperty(prop), filler)
    })

    def individual[_: P]: P[Individual] = P(iri.map(NamedIndividual) | anonymousIndividual)

    def dlSafeRule[_: P]: P[SWRLRule] = P(("DLSafeRule" ~ "(" ~/ annotations ~ "Body" ~ "(" ~ atom.rep ~ ")" ~ "Head" ~ "(" ~ atom.rep ~ ")" ~ ")").map {
      case (anns, body, head) => SWRLRule(body.toSet, head.toSet, anns.toSet)
    })

    def atom[_: P]: P[Atom] = P(classAtom | dataRangeAtom | objectPropertyAtom | dataPropertyAtom | builtInAtom | sameIndividualAtom | differentIndividualsAtom)

    def individualVariable[_: P]: P[Variable] = P(("IndividualVariable" ~ "(" ~/ iri ~ ")").map(Variable))

    def literalVariable[_: P]: P[Variable] = P(("LiteralVariable" ~ "(" ~/ iri ~ ")").map(Variable))

    def iArg[_: P]: P[IArg] = P(individual | individualVariable)

    def dArg[_: P]: P[DArg] = P(literal | literalVariable)

    def classAtom[_: P]: P[ClassAtom] = P(("ClassAtom" ~ "(" ~/ classExpression ~ iArg ~ ")").map {
      case (cls, arg) => ClassAtom(cls, arg)
    })

    def dataRangeAtom[_: P]: P[DataRangeAtom] = P(("DataRangeAtom" ~ "(" ~/ dataRange ~ dArg ~ ")").map {
      case (dr, arg) => DataRangeAtom(dr, arg)
    })

    def objectPropertyAtom[_: P]: P[ObjectPropertyAtom] = P(("ObjectPropertyAtom" ~ "(" ~/ objectPropertyExpression ~ iArg ~ iArg ~ ")").map {
      case (prop, arg1, arg2) => ObjectPropertyAtom(prop, arg1, arg2)
    })

    def dataPropertyAtom[_: P]: P[DataPropertyAtom] = P(("DataPropertyAtom" ~ "(" ~/ iri ~ iArg ~ dArg ~ ")").map {
      case (prop, arg1, arg2) => DataPropertyAtom(DataProperty(prop), arg1, arg2)
    })

    def builtInAtom[_: P]: P[BuiltInAtom] = P(("BuiltInAtom" ~ "(" ~/ iri ~ dArg.rep(1) ~ ")").map {
      case (builtIn, args) => BuiltInAtom(builtIn, args.toList)
    })

    def sameIndividualAtom[_: P]: P[SameIndividualAtom] = P(("SameIndividualAtom" ~ "(" ~/ iArg ~ iArg ~ ")").map {
      case (arg1, arg2) => SameIndividualAtom(arg1, arg2)
    })

    def differentIndividualsAtom[_: P]: P[DifferentIndividualsAtom] = P(("DifferentIndividualsAtom" ~/ "(" ~ iArg ~ iArg ~ ")").map {
      case (arg1, arg2) => DifferentIndividualsAtom(arg1, arg2)
    })

  }

}
