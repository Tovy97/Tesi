package struttureDati.minHeap.BH

import struttureDati.minHeap.MinHeap

import scala.annotation.tailrec

/**
  * Implementa gli heap binomiali che sono un insieme di alberi binomiali, con le seguenti
  * proprietà:
  *
  * 1) Non ci sono due alberi binomiali con lo stesso grado nell’insieme.
  * 2) Gli alberi sono ordinati per rango in ordine crescente
  * 3) Per ogni albero binomiale vale la seguente proprietà: se P è un nodo genitore
  * di C, il valore di P è minore o uguale al valore di C (proprietà dei min-heap).
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @param trees è l'iniseme di alberi binomiali
  * @param ord   è la classe contenente il criterio di ordinamento del tipo parametrico.
  * @tparam E indica il tipo di elementi contenuti nel BinomialHeap. Deve essere ordinabile ed è invariante.
  * @throws IllegalArgumentException se il parametro ord è null
  */
@throws(classOf[IllegalArgumentException])
final case class BinomialHeap[E](private val trees: List[BinomialTree[E]])(implicit ord: Ordering[E]) extends MinHeap[E] {
  require(!(ord eq null), "Il tipo deve essere ordinabile")

  /**
    * Ritorna il numero di elementi presenti nell'heap binomiale.
    *
    * @return il numero di elementi presenti nell'heap binomiale.
    */
  override lazy val size: Int = trees.map(x => x.size).sum

  /**
    * Controlla se è presente almeno un elemento nell'heap binomiale.
    *
    * @return true se l'heap binomiale è vuoto, altrimenti false
    */
  override lazy val isEmpty: Boolean = trees.isEmpty

  /**
    * Inserisce un elemento nell'heap binomiale.
    * Complessità: O(log(n)) nel caso peggiore.
    * Complessità ammortizzata: O(1)
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return l'heap binomiale in cui è stato inserito l'elemento
    */
  override def insert(el: E)(implicit ord: Ordering[E]): BinomialHeap[E] = insertTree(BinomialTree(el, 0, Nil), this)

  /**
    * Unisce gli elementi presenti in due heap binomiale in un unico heap binomiale.
    * Se l'heap passato come parametro non è un'istanza di BinomialHeap viene sollevata un'eccezione.
    * Complessità: O(log(n)) nel caso peggiore.
    * Complessità ammortizzata: O(log(n))
    *
    * @param hp  è l'heap binomiale da unire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return l'heap binomiale unione
    * @throws IllegalArgumentException se l'heap hp passato come parametro non è un heap binomiale
    */
  @throws(classOf[IllegalArgumentException])
  override def merge(hp: MinHeap[E])(implicit ord: Ordering[E]): BinomialHeap[E] = hp match {
    case lh: BinomialHeap[E] => mrg(lh, this)
    case _ => throw new IllegalArgumentException("Impossibile eseguire il merge tra un BinomialHeap e un altro tipo di Heap")
  }

  /**
    * Ritorna l'elemento minore presente nell'heap binomiale.
    * Se l'heap binomiale è vuoto viene sollevata un'eccezione.
    * Complessità: O(log(n)) nel caso peggiore.
    * Complessità ammortizzata: O(log(n))
    *
    * @return l'elemento minore presente nell'heap binomiale.
    * @throws NoSuchElementException se l'heap binomiale è vuoto.
    */
  @throws(classOf[NoSuchElementException])
  override def findMin: E = try {
    removeMinTree(this)._1.el
  } catch {
    case _: NoSuchElementException => throw new NoSuchElementException("Empty().findMin")
  }

  /**
    * Elimina l'elemento minore presente nell'heap binomiale.
    * Complessità: O(log(n)) nel caso peggiore.
    * Complessità ammortizzata: O(log(n))
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return l'heap binomiale in cui è stato eliminato l'elemento minore.
    */
  override def deleteMin(implicit ord: Ordering[E]): BinomialHeap[E] = {
    try {
      val (min, minTree) = removeMinTree(this)
      mrg(BinomialHeap(min.children.reverse), minTree)
    } catch {
      case _: NoSuchElementException => this
    }
  }

  /**
    * Controlla se le 3 proprietà degli heap binomiali e le proprietà degli alberi binomiali sono
    * rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return true se le 3 proprietà degli heap binomiali e le proprietà degli alberi binomiali sono rispettate, altrimenti false.
    */
  override def isCorrect(implicit ord: Ordering[E]): Boolean = {

    @tailrec
    def checkRank(bt1: BinomialTree[E], bt2: BinomialTree[E], l: List[BinomialTree[E]]): Boolean = l match {
      case Nil => bt1.rank < bt2.rank
      case h :: Nil => bt1.rank < bt2.rank && bt2.rank < h.rank
      case h1 :: h2 :: t => bt1.rank < bt2.rank && bt2.rank < h1.rank && checkRank(h1, h2, t)
    }

    trees match {
      case Nil => true
      case h :: Nil => h.isCorrect
      case h1 :: h2 :: t => checkRank(h1, h2, t) && trees.forall(x => x.isCorrect)
    }
  }

  /**
    * Ritorna la lista contenente gli elementi degli alberi presenti nell'heap binomiale,
    * scorrendo gli alberi binomiali secondo il grado in ordine crescente.
    *
    * @return la lista contenente gli elementi degli alberi presenti nell'heap binomiale, scorrendo gli alberi secondo il grado in ordine crescente.
    */
  override lazy val toList: List[E] = trees.flatMap(x => x.toList)

  /**
    * Ritorna la stringa che rappresenta l'heap binomiale.
    *
    * @return la stringa che rappresenta l'heap binomiale.
    */
  override lazy val toString: String = {
    def listToString(l: List[BinomialTree[E]]): String = l match {
      case Nil => ""
      case h :: Nil => h.toString
      case h :: t => h + ", " + listToString(t)
    }

    "BinomialHeap(" + listToString(trees) + ")"
  }

  /**
    * Implementa l'unione tra due heap binomiali.
    *
    * @param bh1 il primo heap binomiali.
    * @param bh2 il secondo heap binomiali.
    * @return il heap binomiali risultato dell'unione dei due heap binomiali passati per parametro.
    */
  private def mrg(bh1: BinomialHeap[E], bh2: BinomialHeap[E]): BinomialHeap[E] = (bh1, bh2) match {
    case (t, BinomialHeap(Nil)) => t
    case (BinomialHeap(Nil), t) => t
    case (BinomialHeap(h1 :: t1), BinomialHeap(h2 :: t2)) => if (h1.rank < h2.rank) {
      BinomialHeap(h1 :: mrg(BinomialHeap(t1), bh2).trees)
    } else if (h1.rank > h2.rank) {
      BinomialHeap(h2 :: mrg(BinomialHeap(t2), bh1).trees)
    } else {
      insertTree(mergeTree(h1, h2), mrg(BinomialHeap(t1), BinomialHeap(t2)))
    }
  }

  /**
    * Rimuove dall'insieme di alberi dell'heap binomiale l'albero che ha come radice
    * l'elemento minore (e quindi l'albero che ha l'elemento minore in assoluto
    * nell'heap binomiale) e la coppia formata dall'albero binomiale estratto e dal
    * resto dell'heap binomiale.
    * Se l'heap binomiale è vuoto viene sollevata un'eccezione.
    *
    * @param bh  l'heap binomiale da cui estrarre l'albero con l'elemento minore.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return la coppia formata dall'albero binomiale estratto e dal resto dell'heap binomiale.
    * @throws NoSuchElementException se l'heap binomiale è vuoto.
    */
  @throws(classOf[NoSuchElementException])
  private def removeMinTree(bh: BinomialHeap[E])(implicit ord: Ordering[E]): (BinomialTree[E], BinomialHeap[E]) = bh match {
    case BinomialHeap(Nil) => throw new NoSuchElementException()
    case BinomialHeap(x :: Nil) => (x, BinomialHeap(Nil))
    case BinomialHeap(h :: t) =>
      val (bt1, bh1) = removeMinTree(BinomialHeap(t))
      if (ord.lteq(h.el, bt1.el)) {
        (h, BinomialHeap(t))
      } else {
        (bt1, BinomialHeap(h :: bh1.trees))
      }
  }

  /**
    * Implementa l'inserimento di un albero binomiale nell'heap binomiale.
    *
    * @param bt  è l'albero binomiale da inserire.
    * @param bh  è l'heap binomiale in cui aggiungere l'albero.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return l'heap binomiale in cui è stato inserito l'albero binomiale.
    */
  @tailrec
  private def insertTree(bt: BinomialTree[E], bh: BinomialHeap[E])(implicit ord: Ordering[E]): BinomialHeap[E] = {
    bh match {
      case BinomialHeap(Nil) => BinomialHeap(List(bt))
      case BinomialHeap(h :: t) => if (bt.rank < h.rank) {
        BinomialHeap(bt :: bh.trees)
      } else {
        insertTree(mergeTree(bt, h), BinomialHeap(t))
      }
    }
  }

  /**
    * Effettua l'unione di due alberi binomiali con lo stesso grado.
    * Se gli alberi binomiali hanno gradi diversi viene sollevata un'eccezione.
    *
    * @param bt1 il primo albero binomiale
    * @param bt2 il secondo albero binomiale
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return l'albero binomiale risultante dall'unione dei due alberi binomiali passati per parametro.
    * @throws IllegalArgumentException se i due alberi binomiali hanno gradi diversi.
    */
  @throws(classOf[IllegalArgumentException])
  private def mergeTree(bt1: BinomialTree[E], bt2: BinomialTree[E])(implicit ord: Ordering[E]): BinomialTree[E] = {
    require(bt1.rank == bt2.rank, "Gli alberi binomiali possono essere uniti solo se hanno lo stesso grado")
    if (ord.lteq(bt1.el, bt2.el)) {
      BinomialTree(bt1.el, bt1.rank + 1, bt2 :: bt1.children)
    } else {
      BinomialTree(bt2.el, bt1.rank + 1, bt1 :: bt2.children)
    }
  }
}

/**
  * Companion Object del trait BinomialHeap.
  * Permette la creazione di heap binomiali.
  */
object BinomialHeap {
  /**
    * Permette la creazione di un heap binomiale che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nell'heap binomiale
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato l'heap binomiale.
    * @return l'heap binomiale contenente gli elementi passati come parametro
    * @throws IllegalArgumentException se il parametro ord è null
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): BinomialHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    els.foldLeft(BinomialHeap[E](Nil))((x, y) => x insert y)
  }
}