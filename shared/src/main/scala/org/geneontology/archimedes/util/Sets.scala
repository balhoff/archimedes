package org.geneontology.archimedes.util

object Sets {

  sealed trait NonEmptySet[A] {

    def items: Set[A]

    def +(item: A): NonEmptySet[A] = NonEmptySet.unsafeMake(items + item)

    def ++(other: NonEmptySet[A]): NonEmptySet[A] = NonEmptySet.unsafeMake(items ++ other.items)

    def ++(other: IterableOnce[A]): NonEmptySet[A] = NonEmptySet.unsafeMake(items ++ other)

    override def toString: String = s"NonEmptySet(${items})"

    def canEqual(a: Any): Boolean = a.isInstanceOf[NonEmptySet[_]]

    override def equals(that: Any): Boolean = that match {
      case that: NonEmptySet[_] => that.canEqual(this) && this.items == that.items
      case _ => false
    }

    override def hashCode(): Int = items.hashCode

  }

  object NonEmptySet {

    private[Sets] def unsafeMake[A](set: Set[A]): NonEmptySet[A] = new NonEmptySet[A] {
      def items: Set[A] = set
    }

    def create[A](set: Set[A]): Either[IllegalArgumentException, NonEmptySet[A]] =
      if (set.nonEmpty) Right(unsafeMake(set))
      else Left(new IllegalArgumentException(s"Must have at least one element, found ${set.size}"))

  }

  sealed trait PluralSet[A] extends NonEmptySet[A] {

    def items: Set[A]

    override def +(item: A): PluralSet[A] = PluralSet.unsafeMake(items + item)

    override def ++(other: NonEmptySet[A]): PluralSet[A] = PluralSet.unsafeMake(items ++ other.items)

    override def ++(other: IterableOnce[A]): PluralSet[A] = PluralSet.unsafeMake(items ++ other)

    def -(item: A): NonEmptySet[A] = NonEmptySet.unsafeMake(items - item)

    override def toString: String = s"PluralSet(${items})"

  }

  object PluralSet {

    private def unsafeMake[A](set: Set[A]): PluralSet[A] = new PluralSet[A] {
      def items: Set[A] = set
    }

    def create[A](set: Set[A]): Either[IllegalArgumentException, PluralSet[A]] =
      if (set.size > 1) Right(unsafeMake(set))
      else Left(new IllegalArgumentException(s"Must have at least two elements, found ${set.size}"))

  }

}
