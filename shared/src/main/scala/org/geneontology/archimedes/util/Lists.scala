package org.geneontology.archimedes.util

object Lists {

  final case class PluralList[+A](first: A, second: A, rest: List[A]) {

    def toList: List[A] = first :: second :: rest

  }

}
