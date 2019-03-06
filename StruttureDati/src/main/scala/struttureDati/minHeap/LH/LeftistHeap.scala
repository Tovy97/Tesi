package struttureDati.minHeap.LH

import struttureDati.minHeap.MinHeap

/**
  * Implementa un particolare tipo di heap: gli leftist-heap.
  * Gli leftist-heap sono alberi binari nei quali:
  *
  * 1) se P è un nodo genitore di C, il valore di P è minore o uguale al valore di C (proprietà dei min-heap)
  * 2) il grado di ogni figlio sinistro è almeno pari a quello del suo fratello destro,
  * dove per grado di un nodo si intente la lunghezza del percorso più a destra dal
  * nodo stesso verso un nodo vuoto.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @tparam E indica il tipo di elementi contenuti nel LeftistHeap. Deve essere ordinabile ed è invariante.
  */
sealed trait LeftistHeap[E] extends MinHeap[E] {

  /**
    * Questo campo indica il grado del nodo, dove per grado di un nodo si intente
    * la lunghezza del percorso più a destra dal nodo stesso verso un nodo vuoto.
    *
    * @return il grado del nodo.
    */
  protected def rank: Int

  /**
    * Ritorna il numero di elementi presenti nel leftist-heap.
    *
    * @return il numero di elementi presenti nel leftist-heap.
    */
  override final lazy val size: Int = this match {
    case Empty() => 0
    case Node(_, _, sx, dx) => sx.size + dx.size + 1
  }

  /**
    * Controlla se è presente almeno un elemento nel leftist-heap.
    *
    * @return true se il leftist-heap è vuoto, altrimenti false
    */
  override final lazy val isEmpty: Boolean = this match {
    case Empty() => true
    case Node(_, _, _, _) => false
  }

  /**
    * Inserisce un elemento nel leftist-heap.
    * Complessità: O(log(n)) nel caso peggiore.
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return il leftist-heap in cui è stato inserito l'elemento
    * @throws IllegalArgumentException se il parametro ord è null
    */
  @throws(classOf[IllegalArgumentException])
  override final def insert(el: E)(implicit ord: Ordering[E]): LeftistHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    merge(Node(el, 1, Empty(), Empty()))
  }

  /**
    * Unisce gli elementi presenti in due leftist-heap in un unico leftist-heap.
    * Se l'heap passato come parametro non è un'istanza di LeftistHeap viene sollevata un'eccezione.
    * Complessità: O(log(n)) nel caso peggiore.
    *
    * @param hp  è il leftist-heap da unire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return il leftist-heap unione
    * @throws IllegalArgumentException se il parametro ord è null
    * @throws IllegalArgumentException se l'heap hp passato come parametro non è un leftist-heap
    */
  @throws(classOf[IllegalArgumentException])
  override final def merge(hp: MinHeap[E])(implicit ord: Ordering[E]): LeftistHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    hp match {
      case lh: LeftistHeap[E] => mrg(lh, this)
      case _ => throw new IllegalArgumentException("Impossibile eseguire il merge tra un LeftistHeap e un altro tipo di Heap")
    }
  }

  /**
    * Ritorna l'elemento minore presente nel leftist-heap.
    * Se il leftist-heap è vuoto viene sollevata un'eccezione.
    * Complessità: O(1) nel caso peggiore.
    *
    * @return l'elemento minore presente nel leftist-heap.
    * @throws NoSuchElementException se il leftist-heap è vuoto.
    */
  @throws(classOf[NoSuchElementException])
  override final def findMin: E = this match {
    case Empty() => throw new NoSuchElementException("Empty.findMin")
    case Node(x, _, _, _) => x
  }

  /**
    * Elimina l'elemento minore presente nel leftist-heap.
    * Complessità: O(log(n)) nel caso peggiore.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return il leftist-heap in cui è stato eliminato l'elemento minore.
    * @throws IllegalArgumentException se il parametro ord è null
    */
  @throws(classOf[IllegalArgumentException])
  override final def deleteMin(implicit ord: Ordering[E]): LeftistHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    this match {
      case Empty() => this
      case Node(_, _, sx, dx) => mrg(sx, dx)
    }
  }

  /**
    * Controlla se le 2 proprietà degli leftist-heap sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return true se le 2 proprietà degli leftist-heap sono rispettate, altrimenti false.
    * @throws IllegalArgumentException se il parametro ord è null
    */
  @throws(classOf[IllegalArgumentException])
  override final def isCorrect(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    this match {
      case Empty() => true
      case Node(el, _, sx, dx) => (sx, dx) match {
        case (Empty(), Empty()) => true
        case (Node(elSx, _, _, _), Empty()) => ord.lteq(el, elSx) && sx.isCorrect
        case (Empty(), Node(_, _, _, _)) => false
        case (Node(elSx, rSx, _, _), Node(elDx, rDx, _, _)) => ord.lteq(el, elSx) && ord.lteq(el, elDx) && rSx >= rDx && sx.isCorrect && dx.isCorrect
      }
    }
  }

  /**
    * Ritorna la lista contenente gli elementi del leftist-heap secondo la strategia pre-order.
    *
    * @return la lista contenente gli elementi del leftist-heap secondo la strategia pre-order.
    */
  override final lazy val toList: List[E] = {
    def createList(lh: LeftistHeap[E], temp: List[E]): List[E] = lh match {
      case Empty() => temp
      case Node(el, _, sx, dx) => el :: createList(sx, createList(dx, temp))
    }

    createList(this, Nil)
  }

  /**
    * Ritorna la stringa che rappresenta il leftist-heap.
    *
    * @return la stringa che rappresenta l'leftist-heap.
    */
  override final lazy val toString: String = {
    def treeToString(lh: LeftistHeap[E]): String = lh match {
      case Empty() => "."
      case Node(el, _, sx, dx) => "(" + treeToString(sx) + el + treeToString(dx) + ")"
    }

    "LeftistHeap(" + treeToString(this) + ")"
  }

  /**
    * Implementa l'unione tra due leftist-heap.
    *
    * @param lh1 il primo leftist-heap.
    * @param lh2 il secondo leftist-heap.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return il leftist-heap risultato dell'unione dei due leftist-heap passati per parametro.
    */
  private final def mrg(lh1: LeftistHeap[E], lh2: LeftistHeap[E])(implicit ord: Ordering[E]): LeftistHeap[E] = {
    def makeT(el: E, lh1: LeftistHeap[E], lh2: LeftistHeap[E]): LeftistHeap[E] = {
      if (lh1.rank >= lh2.rank) {
        Node(el, lh2.rank + 1, lh1, lh2)
      } else {
        Node(el, lh1.rank + 1, lh2, lh1)
      }
    }

    (lh1, lh2) match {
      case (h, Empty()) => h
      case (Empty(), h) => h
      case (Node(x, _, sx1, dx1), Node(y, _, sx2, dx2)) => if (ord.lt(x, y)) {
        makeT(x, sx1, mrg(dx1, lh2))
      } else {
        makeT(y, sx2, mrg(dx2, lh1))
      }
    }
  }
}

/**
  * Companion Object del trait LeftistHeap.
  * Permette la creazione di leftist-heap.
  */
object LeftistHeap {
  /**
    * Permette la creazione di un leftist-heap che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nel leftist-heap
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato il leftist-heap.
    * @return il leftist-heap contenente gli elementi passati come parametro
    * @throws IllegalArgumentException se il parametro ord è null
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): LeftistHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    if (els.isEmpty) {
      Empty()
    } else {
      els.foldLeft(LeftistHeap())((x, y) => x insert y)
    }
  }
}

/**
  * Rappresenta un leftist-heap non vuoto con un elemento nella radice e due sottoalberi.
  *
  * @param el   è l'elemento contenuto nel nodo radice.
  * @param rank è il grado del nodo radice.
  * @param sx   è il sottoalbero sinistro del nodo radice.
  * @param dx   è il sottoalbero destro del nodo radice.
  * @tparam E indica il tipo di elementi contenuti nel LeftistHeap. Deve essere ordinabile ed è invariante.
  */
private final case class Node[E](el: E, rank: Int, sx: LeftistHeap[E], dx: LeftistHeap[E]) extends LeftistHeap[E]

/**
  * Rappresenta un leftist-heap vuoto (una foglia).
  *
  * @tparam E indica il tipo di elementi contenuti nel LeftistHeap. Deve essere ordinabile ed è invariante.
  */
private final case class Empty[E]() extends LeftistHeap[E] {
  override val rank: Int = 0
}
