package struttureDati.splayHeap.SH

import scala.annotation.tailrec

/**
  * Implementa gli splay-heap che sono particolari alberi binari di ricerca nei quali:
  *
  * 1) Sono consentiti elementi ripetuti
  * 2) I due sottoalberi di un nodo sono a loro volta splay-heap.
  * 3) l sottoalbero sinistro di un nodo contiene soltanto i nodi con valori minori o uguali del nodo stesso.
  * 4) Il sottoalbero destro di un nodo contiene soltanto i nodi con valori maggiori o uguali del nodo stesso.
  * 5) Quando si inserisce un nuovo nodo esso diventa sempre la nuova radice dell’albero.
  * 6) Sia l’inserimento che la cancellazione ristrutturano l’albero con la seguente regola: se
  * giungiamo ad un nodo X seguendo il percorso di sinistra (o di destra) da un nodo Y (il padre
  * di X) e se dobbiamo proseguire il percorso andando a sinistra (o a destra), allora, prima di
  * procedere il cammino, ruotiamo X e Y.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @tparam E indica il tipo di elementi contenuti nel SplayHeap. Deve essere ordinabile ed è invariante.
  */
sealed trait SplayHeap[E] {

  /**
    * Ritorna il numero di elementi presenti nell'heap.
    *
    * @return il numero di elementi presenti nell'heap.
    */
  final lazy val size: Int = this match {
    case Empty() => 0
    case Node(_, sx, dx) => sx.size + dx.size + 1
  }

  /**
    * Controlla se è presente almeno un elemento nel splay-heap.
    *
    * @return true se lo splay-heap è vuoto, altrimenti false
    */
  final lazy val isEmpty: Boolean = this match {
    case Empty() => true
    case Node(_, _, _) => false
  }

  /**
    * Inserisce un elemento nel splay-heap e ristruttura l'albero secondo la proprietà 6.
    * Complessità: O(n) nel caso peggiore.
    * Complessità ammortizzata: O(log(n)).
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return lo splay-heap ristrutturato secondo la proprietà 6 in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  final def insert(el: E)(implicit ord: Ordering[E]): SplayHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    val (a, b) = partition(el, this)
    Node(el, a, b)
  }

  /**
    * Unisce gli elementi presenti in due splay-heap in un unico splay-heap.
    * Complessità: O(n) nel caso peggiore.
    * Complessità ammortizzata: O(n).
    *
    * @param sh  è lo splay-heap da unire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return lo splay-heap unione
    */
  @throws(classOf[IllegalArgumentException])
  final def merge(sh: SplayHeap[E])(implicit ord: Ordering[E]): SplayHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def mrg(sh1: SplayHeap[E], sh2: SplayHeap[E]): SplayHeap[E] = sh1 match {
      case Empty() => sh2
      case Node(x, a, b) =>
        val (ta, tb) = partition(x, sh2)
        Node(x, mrg(ta, a), mrg(tb, b))
    }

    mrg(sh, this)
  }

  /**
    * Ritorna l'elemento minore presente nello splay-heap.
    * Se il splay-heap è vuoto viene sollevata un'eccezione.
    * Complessità: O(n) nel caso peggiore.
    * Complessità ammortizzata: O(log(n)).
    *
    * @throws java.util.NoSuchElementException se lo splay-heap è vuoto.
    * @return l'elemento minore presente nello splay-heap.
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  final def findMin: E = this match {
    case Empty() => throw new NoSuchElementException("Empty.findMin")
    case Node(e, Empty(), _) => e
    case Node(_, s, _) => s.findMin
  }

  /**
    * Elimina l'elemento minore presente nello splay-heap e ristruttura l'albero
    * secondo la proprietà 6.
    * Complessità: O(n) nel caso peggiore.
    * Complessità ammortizzata: O(log(n)).
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return lo splay-heap ristrutturato  secondo la proprietà 6 in cui è stato eliminato l'elemento minore.
    */
  @throws(classOf[IllegalArgumentException])
  final def deleteMin(implicit ord: Ordering[E]): SplayHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    this match {
      case Empty() => this
      case Node(_, Empty(), b) => b
      case Node(y, Node(_, Empty(), b), c) => Node(y, b, c)
      case Node(y, Node(x, a, b), c) => Node(x, a.deleteMin, Node(y, b, c))
    }
  }

  /**
    * Controlla se le 6 proprietà degli splay-heap sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le 6 proprietà degli splay-heap sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  final def isCorrect(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def check(bst: SplayHeap[E]): Boolean = bst match {
      case Empty() => true
      case Node(el, sx, dx) => sx.toList.forall(x => ord.lteq(x, el)) && dx.toList.forall(x => ord.gteq(x, el)) && check(sx) && check(dx)
    }

    check(this)
  }

  /**
    * Ritorna la lista ordinata contenente gli elementi dello splay-heap.
    *
    * @return la lista ordinata contenente gli elementi dello splay-heap.
    */
  final lazy val toList: List[E] = {
    def createList(rbt: SplayHeap[E], temp: List[E]): List[E] = rbt match {
      case Empty() => temp
      case Node(el, sx, dx) => createList(sx, el :: createList(dx, temp))
    }

    createList(this, Nil)
  }

  /**
    * Ritorna la stringa che rappresenta lo splay-heap.
    *
    * @return la stringa che rappresenta lo splay-heap.
    */
  override final lazy val toString: String = {
    def treeToString(sh: SplayHeap[E]): String = sh match {
      case Empty() => "."
      case Node(el, sx, dx) => "(" + treeToString(sx) + el + treeToString(dx) + ")"
    }

    "SplayHeap(" + treeToString(this) + ")"
  }

  /**
    * Divide lo splay-heap in due splay-heap, il primo contenente solo elementi minori uguali
    * al pivot, il secondo solo quelli maggiori al pivot. I due splay-heap ritornati sono
    * ristrutturati secondo la proprietà 6.
    *
    * @param pivot è l'elemento che decide come creare i due splay-heap
    * @param sh    è lo splay-heap da dividere
    * @param ord   è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @return la coppia di splay-heap dove il primo contenente solo elementi minori uguali al pivot e il secondo solo quelli maggiori al pivot.
    */
  private final def partition(pivot: E, sh: SplayHeap[E])(implicit ord: Ordering[E]): (SplayHeap[E], SplayHeap[E]) = (pivot, sh) match {
    case (_, Empty()) => (Empty(), Empty())
    case (_, Node(x, a, b)) => if (ord.lteq(x, pivot)) {
      b match {
        case Empty() => (sh, Empty())
        case Node(y, b1, b2) => if (ord.lteq(y, pivot)) {
          val (small, big) = partition(pivot, b2)
          (Node(y, Node(x, a, b1), small), big)
        } else {
          val (small, big) = partition(pivot, b1)
          (Node(x, a, small), Node(y, big, b2))
        }
      }
    } else {
      a match {
        case Empty() => (Empty(), sh)
        case Node(y, a1, a2) => if (ord.lteq(y, pivot)) {
          val (small, big) = partition(pivot, a2)
          (Node(y, a1, small), Node(x, big, b))
        } else {
          val (small, big) = partition(pivot, a1)
          (small, Node(y, big, Node(x, a2, b)))
        }
      }
    }
  }
}

/**
  * Companion Object del trait SplayHeap.
  * Permette la creazione di splay-heap.
  */
object SplayHeap {
  /**
    * Permette la creazione di un splay-heap che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nel splay-heap
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato lo splay-heap.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return lo splay-heap contenente gli elementi passati come parametro
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): SplayHeap[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    if (els.isEmpty) {
      Empty()
    } else {
      els.foldLeft(SplayHeap())((x, y) => x insert y)
    }
  }
}

/**
  * Rappresenta uno splay-heap non vuoto con un elemento nella radice e due sottoalberi.
  *
  * @param el è l'elemento contenuto nel nodo radice.
  * @param sx è il sottoalbero sinistro del nodo radice.
  * @param dx è il sottoalbero destro del nodo radice.
  * @tparam E indica il tipo di elementi contenuti nel SplayHeap. Deve essere ordinabile ed è invariante.
  */
private final case class Node[E](el: E, sx: SplayHeap[E], dx: SplayHeap[E]) extends SplayHeap[E]

/**
  * Rappresenta uno splay-heap vuoto (una foglia).
  *
  * @tparam E indica il tipo di elementi contenuti nel SplayHeap. Deve essere ordinabile ed è invariante.
  */
private final case class Empty[E]() extends SplayHeap[E]
