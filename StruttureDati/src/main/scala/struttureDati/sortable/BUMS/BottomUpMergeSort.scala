package struttureDati.sortable.BUMS

import struttureDati.sortable.Sortable

import scala.annotation.tailrec

/**
  * Implementa un collezione di elementi ordinabili che devono essere trasformati in una lista
  * ordinata utilizzando il merge sort.
  * La collezione è organizzata con uno stream di segmenti, dove i segmenti sono una lista
  * di elementi già ordinati.
  * La collezione ha la seguente proprietà:
  *
  * 1) nello stream non si cono due segmenti con lo stesso numeri di elementi.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @param size     il numero di elementi presenti nella collezione.
  * @param segments stream contenente gli elementi della collezione organizzati in Segmenti. Non ci sono due segmenti con lo stesso numero di elementi.
  * @param ord      è la classe contenente il criterio di ordinamento del tipo parametrico.
  * @tparam E indica il tipo di elementi contenuti nel BottomUpMergeSort. Deve essere ordinabile ed è invariante.
  * @throws IllegalArgumentException se il parametro ord è null
  */
@throws(classOf[IllegalArgumentException])
final case class BottomUpMergeSort[E](override val size: Int, private val segments: Stream[Segment[E]])(implicit ord: Ordering[E]) extends Sortable[E] {
  require(!(ord eq null), "Il tipo deve essere ordinabile")

  /**
    * Inserisce un elemento nella collezione.
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return la collezione in cui è stato inserito l'elemento
    */
  override def add(el: E)(implicit ord: Ordering[E]): BottomUpMergeSort[E] = {
    @tailrec
    def addSeg(seg: Segment[E], segs: Stream[Segment[E]], size: Int): Stream[Segment[E]] = {
      if (size % 2 == 0) {
        seg +: segs
      } else {
        addSeg(merge(seg, segs.head), segs.tail, size / 2)
      }
    }

    BottomUpMergeSort(size + 1, addSeg(Segment(el), segments, size))
  }

  /**
    * Controlla se è presente almeno un elemento nella collezione.
    *
    * @return true se la collezione è vuota, altrimenti false
    */
  override lazy val isEmpty: Boolean = size == 0

  /**
    * Ordina la collezione e restituisce la lista ordinata con gli elementi della collezione.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return la lista ordinata con gli elementi della collezione.
    */
  override def sort(implicit ord: Ordering[E]): List[E] = {
    @tailrec
    def mergeAll(xs: Segment[E], ys: Stream[Segment[E]]): Segment[E] = (xs, ys) match {
      case (_, Stream.Empty) => xs
      case (_, seg +: segs) => mergeAll(merge(xs, seg), segs)
    }

    mergeAll(Segment(), segments).seg
  }

  /**
    * Controlla se le proprietà della collezione sono rispettate, altrimenti false.
    *
    * @return true se le proprietà della collezione sono rispettate, altrimenti false.
    */
  override def isCorrect: Boolean = segments.forall(s => segments.count(x => x.seg.size == s.seg.size) == 1)

  /**
    * Ritorna la stringa che rappresenta la collezione.
    *
    * @return la stringa che rappresenta la collezione.
    */
  override lazy val toString: String = {
    def streamToString(segs: Stream[Segment[E]]): String = segs match {
      case Stream.Empty => ""
      case h +: Stream.Empty => h.toString
      case h +: t => h + ", " + streamToString(t)
    }

    "BottomUpMergeSort(" + streamToString(segments) + ")"
  }

  /**
    * Unisce due segmenti.
    *
    * @param seg1 il primo segmento da unire
    * @param seg2 il primo segmento da unire
    * @param ord  è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return il segmento unione dei due passati per parametro
    */
  private def merge(seg1: Segment[E], seg2: Segment[E])(implicit ord: Ordering[E]): Segment[E] = (seg1, seg2) match {
    case (Segment(Nil), _) => seg2
    case (_, Segment(Nil)) => seg1
    case (Segment(xs@x :: xs1), Segment(ys@y :: ys1)) => if (ord.lteq(x, y)) {
      Segment(x :: merge(Segment(xs1), Segment(ys)).seg)
    } else {
      Segment(y :: merge(Segment(xs), Segment(ys1)).seg)
    }
  }
}

/**
  * Companion Object del trait BinarySearchTree.
  * Permette la creazione di collezioni.
  */
object BottomUpMergeSort {
  /**
    * Permette la creazione di una collezione che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nella collezione.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato la collezione.
    * @return la collezione contenente gli elementi passati come parametro
    * @throws IllegalArgumentException se il parametro ord è null
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): BottomUpMergeSort[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    els.foldLeft(BottomUpMergeSort[E](0, Stream.Empty))((x, y) => x add y)
  }
}