package org.geneontology.archimedes.owl

object OWLOps {

  implicit class HasSignature(val self: OWLObject) extends AnyVal {

    def signature: Set[Entity] = self match {
      case e: Entity                                    => Set(e)
      case ObjectSomeValuesFrom(p, f)                   => p.signature ++ f.signature
      case ObjectAllValuesFrom(p, f)                    => p.signature ++ f.signature
      case ObjectIntersectionOf(operands)               => operands.items.flatMap(_.signature)
      case ObjectUnionOf(operands)                      => operands.items.flatMap(_.signature)
      case ObjectOneOf(inds)                            => inds.items.flatMap(_.signature)
      case ObjectComplementOf(c)                        => c.signature
      case ObjectHasValue(p, i)                         => p.signature ++ i.signature
      case ObjectHasSelf(p)                             => p.signature
      case ObjectMinCardinality(_, p, f)                => p.signature ++ f.signature
      case ObjectMaxCardinality(_, p, f)                => p.signature ++ f.signature
      case ObjectExactCardinality(_, p, f)              => p.signature ++ f.signature
      case ObjectInverseOf(p)                           => p.signature
      case DataSomeValuesFrom(p, f)                    => p.signature ++ f.signature
      case DataAllValuesFrom(p, f)                     => p.signature ++ f.signature
      case DataIntersectionOf(drs)                      => drs.items.flatMap(_.signature)
      case DataUnionOf(drs)                             => drs.items.flatMap(_.signature)
      case DataComplementOf(dr)                         => dr.signature
      case DataMinCardinality(_, p, v)                  => p.signature ++ v.to(Set).flatMap(_.signature)
      case DataMaxCardinality(_, p, v)                  => p.signature ++ v.to(Set).flatMap(_.signature)
      case DataExactCardinality(_, p, v)                => p.signature ++ v.to(Set).flatMap(_.signature)
      case DataHasValue(p, v)                           => p.signature ++ v.signature
      case DataOneOf(vs)                                => vs.items.flatMap(_.signature)
      case DatatypeDefinition(dt, dr, as)               => dt.signature ++ dr.signature ++ as.flatMap(_.signature)
      case DatatypeRestriction(dt, frs)                 => dt.signature ++ frs.items.flatMap(_.signature)
      case Facet(_)                                     => Set.empty
      case FacetRestriction(f, v)                       => f.signature ++ v.signature
      case HasKey(ce, ops, dps, as)                     => ce.signature ++ ops.flatMap(_.signature) ++ dps.flatMap(_.signature) ++ as.flatMap(_.signature)
      case SubClassOf(sub, sup, as)                     => sub.signature ++ sup.signature ++ as.flatMap(_.signature)
      case EquivalentClasses(operands, as)              => operands.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case DisjointClasses(operands, as)                => operands.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case DisjointUnion(c, operands, as)               => c.signature ++ operands.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case ClassAssertion(c, i, as)                     => c.signature ++ i.signature ++ as.flatMap(_.signature)
      case ObjectPropertyAssertion(p, s, t, as)         => p.signature ++ s.signature ++ t.signature ++ as.flatMap(_.signature)
      case NegativeObjectPropertyAssertion(p, s, o, as) => p.signature ++ s.signature ++ o.signature ++ as.flatMap(_.signature)
      case DataPropertyAssertion(p, s, v, as)           => p.signature ++ s.signature ++ v.signature ++ as.flatMap(_.signature)
      case NegativeDataPropertyAssertion(p, s, v, as)   => p.signature ++ s.signature ++ v.signature ++ as.flatMap(_.signature)
      case SameIndividual(is, as)                       => is.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case DifferentIndividuals(is, as)                 => is.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case ObjectPropertyDomain(p, ce, as)              => p.signature ++ ce.signature ++ as.flatMap(_.signature)
      case ObjectPropertyRange(p, ce, as)               => p.signature ++ ce.signature ++ as.flatMap(_.signature)
      case EquivalentDataProperties(ps, as)             => ps.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case InverseObjectProperties(p1, p2, as)          => p1.signature ++ p2.signature ++ as.flatMap(_.signature)
      case TransitiveObjectProperty(p, as)              => p.signature ++ as.flatMap(_.signature)
      case FunctionalObjectProperty(p, as)              => p.signature ++ as.flatMap(_.signature)
      case ReflexiveObjectProperty(p, as)               => p.signature ++ as.flatMap(_.signature)
      case IrreflexiveObjectProperty(p, as)             => p.signature ++ as.flatMap(_.signature)
      case EquivalentObjectProperties(ps, as)           => ps.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case DisjointObjectProperties(ps, as)             => ps.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case FunctionalDataProperty(p, as)                => p.signature ++ as.flatMap(_.signature)
      case InverseFunctionalObjectProperty(p, as)       => p.signature ++ as.flatMap(_.signature)
      case SymmetricObjectProperty(p, as)               => p.signature ++ as.flatMap(_.signature)
      case AsymmetricObjectProperty(p, as)              => p.signature ++ as.flatMap(_.signature)
      case SubObjectPropertyOf(x, y, as)                => x.signature ++ y.signature ++ as.flatMap(_.signature)
      case ObjectPropertyChain(ps)                      => ps.to(Set).flatMap(_.signature)
      case DataPropertyDomain(p, ce, as)                => p.signature ++ ce.signature ++ as.flatMap(_.signature)
      case DataPropertyRange(p, dr, as)                 => p.signature ++ dr.signature ++ as.flatMap(_.signature)
      case DisjointDataProperties(ps, as)               => ps.items.flatMap(_.signature) ++ as.flatMap(_.signature)
      case SubDataPropertyOf(x, y, as)                  => x.signature ++ y.signature ++ as.flatMap(_.signature)
      case SubAnnotationPropertyOf(x, y, as)            => x.signature ++ y.signature ++ as.flatMap(_.signature)
      case SWRLRule(b, h, as)                           => b.flatMap(_.signature) ++ h.flatMap(_.signature) ++ as.flatMap(_.signature)
      case BuiltInAtom(_, args)                         => args.flatMap(_.signature).to(Set)
      case ClassAtom(ce, arg)                           => ce.signature ++ arg.signature
      case DataRangeAtom(dr, arg)                       => dr.signature ++ arg.signature
      case ObjectPropertyAtom(p, s, o)                  => p.signature ++ s.signature ++ o.signature
      case DataPropertyAtom(p, s, v)                    => p.signature ++ s.signature ++ v.signature
      case SameIndividualAtom(x, y)                     => x.signature ++ y.signature
      case DifferentIndividualsAtom(x, y)               => x.signature ++ y.signature
      case Variable(_)                                  => Set.empty
    }
  }

}