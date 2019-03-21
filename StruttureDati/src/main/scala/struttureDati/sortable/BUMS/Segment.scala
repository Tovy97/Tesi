package struttureDati.sortable.BUMS

/**
  * Implementa una lista ordinata di elementi.
  *
  * @param seg la lista di elementi ordinati
  * @tparam E indica il tipo di elementi contenuti nel Segment. Deve essere ordinabile ed è invariante.
  * @throws java.lang.IllegalArgumentException se il segmento creato non è corretto
  */
@throws(classOf[IllegalArgumentException])
final case class Segment[E](seg: List[E])(implicit ord: Ordering[E]) {
  require(isCorrect, "Il segmento creato non è corretto")

  /**
    * Ritorna la stringa che rappresenta il segmento.
    *
    * @return la stringa che rappresenta il segmento.
    */
  override lazy val toString: String = {
    def listToString(s: List[E]): String = s match {
      case Nil => ""
      case h +: Nil => h.toString
      case h +: t => h + ", " + listToString(t)
    }

    "Segment(" + listToString(seg) + ")"
  }

  lazy val isCorrect: Boolean = seg.sorted == seg
}

/**
  * Companion Object del trait BinarySearchTree.
  * Permette la creazione di segmenti.
  */
object Segment {
  /**
    * Permette la creazione di un segmento che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nel segmento
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato il segmento.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return il segmento contenente gli elementi passati come parametro
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): Segment[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    Segment(els.foldRight(List[E]())((x, y) => x :: y))
  }
}