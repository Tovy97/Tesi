package struttureDati.minHeap.PH

import struttureDati.minHeap.MinHeap

/**
  * Implementa gli pairing-heap che sono alberi n-ari nei quali:
  *
  * 1) se P è un nodo genitore di C, il valore di P è minore o uguale al valore di C (proprietà dei min-heap)
  * 2) Empty, che rappresenta un pairing heap vuoto, non deve mai apparire nella lista dei figli di un
  * albero non vuoto.
  * 3) La cancellazione ristruttura l'albero secondo la seguente procedura: prima si uniscono (merge) i
  * figli della radice a due a due da sinistra verso destra, poi si uniscono (merge) a due a due i
  * risultati da destra verso sinistra.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @tparam E indica il tipo di elementi contenuti nel PairingHeap. Deve essere ordinabile ed è invariante.
  */
sealed trait PairingHeap[E] extends MinHeap[E] {

  /**
    * Ritorna il numero di elementi presenti nel pairing-heap.
    *
    * @return il numero di elementi presenti nel pairing-heap.
    */
  override final lazy val size: Int = this match {
    case Empty() => 0
    case Node(_, t) => t.map(x => x.size).sum + 1
  }

  /**
    * Controlla se è presente almeno un elemento nel pairing-heap
    *
    * @return true se il pairing-heap è vuoto, altrimenti false
    */
  override final lazy val isEmpty: Boolean = this match {
    case Empty() => true
    case Node(_, _) => false
  }

  /**
    * Inserisce un elemento nel pairing-heap.
    * Complessità: O(1) nel caso peggiore.
    * Complessità ammortizzata: O(1).
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return il pairing-heap in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  override final def insert(el: E)(implicit ord: Ordering[E]): PairingHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    mrg(Node(el, Nil), this)
  }

  /**
    * Unisce gli elementi presenti in due pairing-heap in un unico pairing-heap.
    * Se l'heap passato come parametro non è un'istanza di PairingHeap viene sollevata un'eccezione.
    * Complessità: O(1) nel caso peggiore.
    * Complessità ammortizzata: O(1).
    *
    * @param hp  è il pairing-heap da unire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @throws java.lang.IllegalArgumentException se l'heap hp passato come parametro non è un pairing-heap
    * @return il pairing-heap unione
    */
  @throws(classOf[IllegalArgumentException])
  override final def merge(hp: MinHeap[E])(implicit ord: Ordering[E]): PairingHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    hp match {
      case lh: PairingHeap[E] => mrg(lh, this)
      case _ => throw new IllegalArgumentException("Impossibile eseguire il merge tra un PairingHeap e un altro tipo di Heap")
    }
  }

  /**
    * Ritorna l'elemento minore presente nel pairing-heap.
    * Se il pairing-heap è vuoto viene sollevata un'eccezione.
    * Complessità: O(1) nel caso peggiore.
    * Complessità ammortizzata: O(1).
    *
    * @return l'elemento minore presente nel pairing-heap.
    * @throws java.util.NoSuchElementException se il pairing-heap è vuoto.
    */
  @throws(classOf[NoSuchElementException])
  override final def findMin: E = this match {
    case Empty() => throw new NoSuchElementException("Empty.findMin")
    case Node(e, _) => e
  }

  /**
    * Elimina l'elemento minore presente nel pairing-heap (la radice) e ristruttura l'albero
    * secondo la seguente procedura:
    *
    * 1) Si uniscono (merge) i figli della radice a due a due da sinistra verso destra.
    * 2) Si uniscono (merge) a due a due i risultati del passaggio 1 da destra verso sinistra.
    *
    * Complessità: O(n) nel caso peggiore.
    * Complessità ammortizzata: O(log(n)).
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return il pairing-heap in cui è stato eliminato l'elemento minore.
    */
  @throws(classOf[IllegalArgumentException])
  override final def deleteMin(implicit ord: Ordering[E]): PairingHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def mergePair(l: List[PairingHeap[E]]): PairingHeap[E] = l match {
      case Nil => Empty()
      case h :: Nil => h
      case h1 :: h2 :: hs => mrg(mrg(h1, h2), mergePair(hs))
    }

    this match {
      case Empty() => this
      case Node(_, t) => mergePair(t)
    }
  }

  /**
    * Controlla se le 2 proprietà degli pairing-heap sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le 2 proprietà degli pairing-heap sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  override final def isCorrect(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def checkEl(el: E, t: PairingHeap[E]): Boolean = t match {
      case Empty() => false
      case Node(e, _) => ord.lteq(el, e)
    }

    this match {
      case Empty() => true
      case Node(el, trees) => trees.forall(x => checkEl(el, x) && x.isCorrect)
    }
  }

  /**
    * Ritorna la lista contenente gli elementi del pairing-heap scorrendo i figli di un nodo da
    * sinistra a destra e aggiungendo in testa il nodo stesso.
    *
    * @return la lista contenente gli elementi del pairing-heap scorrendo i figli di un nodo da sinistra a destra e aggiungendo in testa il nodo stesso.
    */
  override final lazy val toList: List[E] = {

    def treeToList(t: List[PairingHeap[E]]): List[E] = t match {
      case Nil => Nil
      case _ => t.flatMap(x => x.toList)
    }

    this match {
      case Empty() => Nil
      case Node(el, trees) => el :: treeToList(trees)
    }
  }

  /**
    * Ritorna la stringa che rappresenta il pairing-heap.
    *
    * @return la stringa che rappresenta il pairing-heap.
    */
  override final lazy val toString: String = {
    def listToString(l: List[PairingHeap[E]]): String = l match {
      case Nil => ""
      case h :: Nil => h.toString
      case h :: t => h + ", " + listToString(t)
    }

    this match {
      case Empty() => "PairingHeap()"
      case Node(e, Nil) => "PairingHeap(" + e + ")"
      case Node(e, t) => "PairingHeap(" + e + ", (" + listToString(t) + "))"
    }
  }

  /**
    * Implementa l'unione tra due pairing-heap.
    *
    * @param hp1 il primo pairing-heap
    * @param hp2 il secondo pairing-heap
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return il pairing-heap risultato dell'unione dei due pairing-heap passati per parametro.
    */
  private final def mrg(hp1: PairingHeap[E], hp2: PairingHeap[E])(implicit ord: Ordering[E]): PairingHeap[E] = (hp1, hp2) match {
    case (h, Empty()) => h
    case (Empty(), h) => h
    case (Node(x, hs1), Node(y, hs2)) => if (ord.lteq(x, y)) {
      Node(x, hp2 :: hs1)
    } else {
      Node(y, hp1 :: hs2)
    }
  }
}

/**
  * Companion Object del trait PairingHeap.
  * Permette la creazione di pairing-heap.
  */
object PairingHeap {
  /**
    * Permette la creazione di un pairing-heap che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nel pairing-heap.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato il pairing-heap.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return il pairing-heap contenente gli elementi passati come parametro
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): PairingHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    if (els.isEmpty) {
      Empty()
    } else {
      els.foldLeft(PairingHeap())((x, y) => x insert y)
    }
  }
}

/**
  * Rappresenta un pairing-heap non vuoto con un elemento nella radice e n sottoalberi.
  *
  * @param el    è l'elemento contenuto nel nodo radice.
  * @param trees è l'insieme dei figli del nodo radice.
  * @tparam E indica il tipo di elementi contenuti nel PairingHeap. Deve essere ordinabile ed è invariante.
  */
private final case class Node[E](el: E, trees: List[PairingHeap[E]]) extends PairingHeap[E]

/**
  * Rappresenta un pairing-heap vuoto (una foglia).
  *
  * @tparam E indica il tipo di elementi contenuti nel PairingHeap. Deve essere ordinabile ed è invariante.
  */
private final case class Empty[E]() extends PairingHeap[E]
