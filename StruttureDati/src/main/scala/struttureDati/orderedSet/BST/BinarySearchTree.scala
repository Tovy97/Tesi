package struttureDati.orderedSet.BST

import struttureDati.orderedSet.OrderedSet
import scala.annotation.tailrec

/**
  * Implementa gli alberi binari di ricerca.
  * Un albero binario di ricerca è un albero nel quale:
  *
  * 1) i due sottoalberi di un nodo sono a loro volta alberi binari di ricerca
  * 2) il sottoalbero sinistro di un nodo contiene soltanto i nodi con valori minori del nodo stesso
  * 3) il sottoalbero destro di un nodo contiene soltanto i nodi con valori maggiori del nodo stesso.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @tparam E indica il tipo di elementi contenuti nell'BinarySearchTree. Deve essere ordinabile ed è invariante.
  */
sealed trait BinarySearchTree[E] extends OrderedSet[E] {

  /**
    * Ritorna il numero di elementi presenti nell'albero binario di ricerca.
    *
    * @return il numero di elementi presenti nell'albero binario di ricerca.
    */
  override final lazy val size: Int = this match {
    case Empty() => 0
    case Node(_, sx, dx) => sx.size + dx.size + 1
  }

  /**
    * Controlla se è presente almeno un elemento nell'albero binario di ricerca.
    *
    * @return true se l'albero binario di ricerca è vuoto, altrimenti false
    */
  override final lazy val isEmpty: Boolean = this match {
    case Empty() => true
    case Node(_, _, _) => false
  }

  /**
    * Inserisce un elemento nell'albero binario di ricerca.
    * Se l'elemento è già presente, non viene aggiunto.
    * Complessità: O(n) nel caso peggiore
    *
    * @param El  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'albero binario di ricerca in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  override final def insert(El: E)(implicit ord: Ordering[E]): BinarySearchTree[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    this match {
      case Empty() => Node(El, Empty(), Empty())
      case Node(El, _, _) => this
      case Node(e, sx, dx) => if (ord.lt(El, e)) {
        Node(e, sx.insert(El), dx)
      } else {
        Node(e, sx, dx.insert(El))
      }
    }
  }

  /**
    * Controlla se un elemento è presente nell'albero binario di ricerca.
    * Complessità: O(n) nel caso peggiore
    *
    * @param El  è l'elemento da cercare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se El è contenuto nell'albero binario di ricerca, altrimenti false
    */
  @tailrec
  @throws(classOf[IllegalArgumentException])
  override final def isMember(El: E)(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    this match {
      case Empty() => false
      case Node(El, _, _) => true
      case Node(e, sx, dx) => if (ord.lt(El, e)) {
        sx.isMember(El)
      } else {
        dx.isMember(El)
      }
    }
  }

  /**
    * Elimina un elemento dall'albero binario di ricerca.
    * Complessità: O(n) nel caso peggiore
    *
    * @param El  è l'elemento da eliminare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'albero binario di ricerca in cui è stato eliminato l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  override final def delete(El: E)(implicit ord: Ordering[E]): BinarySearchTree[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    this match {
      case Empty() => this
      case Node(El, Empty(), Empty()) => Empty()
      case Node(El, sx, Empty()) => sx
      case Node(El, Empty(), dx) => dx
      case Node(El, sx, dx) =>
        val min: E = dx.getMin
        Node(min, sx, dx.delete(min))
      case Node(e, sx, dx) => if (ord.lt(El, e)) {
        Node(e, sx.delete(El), dx)
      } else {
        Node(e, sx, dx.delete(El))
      }
    }
  }

  /**
    * Ritorna l'elemento minore presente nell'albero binario di ricerca.
    * Se l'albero binario di ricerca è vuoto viene sollevata un'eccezione.
    * Complessità: O(n) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se l'albero binario di ricerca è vuoto
    * @return l'elemento minore presente nell'albero di ricerca binario.
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  override final def getMin: E = this match {
    case Empty() => throw new NoSuchElementException("Empty.getMin")
    case Node(e, Empty(), _) => e
    case Node(_, sx, _) => sx.getMin
  }

  /**
    * Ritorna l'elemento maggiore presente nell'albero binario di ricerca.
    * Se l'albero binario di ricerca è vuoto viene sollevata un'eccezione.
    * Complessità: O(n) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se l'albero binario di ricerca è vuoto
    * @return l'elemento maggiore presente nell'albero di ricerca binario.
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  override final def getMax: E = this match {
    case Empty() => throw new NoSuchElementException("Empty().getMax")
    case Node(e, _, Empty()) => e
    case Node(_, _, dx) => dx.getMax
  }

  /**
    * Controlla se le 3 proprietà degli alberi binari di ricerca sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le 3 proprietà degli alberi binari di ricerca sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  override final def isCorrect(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def check(bst: BinarySearchTree[E]): Boolean = bst match {
      case Empty() => true
      case Node(el, sx, dx) => sx.toList.forall(x => ord.lt(x, el)) && dx.toList.forall(x => ord.gt(x, el)) && check(sx) && check(dx)
    }

    check(this)
  }

  /**
    * Ritorna la lista ordinata contenente gli elementi dell'albero binario di ricerca.
    * Complessità: O(n) nel caso peggiore
    *
    * @return la lista ordinata contenente gli elementi dell'albero binario di ricerca.
    */
  override final lazy val toList: List[E] = {
    def createList(rbt: BinarySearchTree[E], temp: List[E]): List[E] = rbt match {
      case Empty() => temp
      case Node(el, sx, dx) => createList(sx, el :: createList(dx, temp))
    }

    createList(this, Nil)
  }

  /**
    * Ritorna la stringa che rappresenta l'albero binario di ricerca.
    *
    * @return la stringa che rappresenta l'albero binario di ricerca.
    */
  override final lazy val toString: String = {
    def treeToString(bst: BinarySearchTree[E]): String = bst match {
      case Empty() => "."
      case Node(el, sx, dx) => "(" + treeToString(sx) + el + treeToString(dx) + ")"
    }

    "BinarySearchTree(" + treeToString(this) + ")"
  }
}

/**
  * Companion Object del trait BinarySearchTree.
  * Permette la creazione di alberi binari di ricerca.
  */
object BinarySearchTree {
  /**
    * Permette la creazione di un'albero binario di ricerca che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nell'abero binario di ricerca.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato l'albero binario di ricerca.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'albero binario di ricerca contenente gli elementi passati come parametro
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): BinarySearchTree[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    if (els.isEmpty) {
      Empty()
    } else {
      els.foldLeft(BinarySearchTree())((x, y) => x insert y)
    }
  }
}

/**
  * Rappresenta un albero binario di ricerca non vuoto con un elemento nella radice e due sottoalberi.
  *
  * @param el è l'elemento contenuto nel nodo radice.
  * @param sx è il sottoalbero binario di ricerca sinistro del nodo radice.
  * @param dx è il sottoalbero binario di ricerca destro del nodo radice.
  * @tparam E indica il tipo di elementi contenuti nell'BinarySearchTree. Deve essere ordinabile ed è invariante.
  */
private final case class Node[E](el: E, sx: BinarySearchTree[E], dx: BinarySearchTree[E]) extends BinarySearchTree[E]

/**
  * Rappresenta un albero binario di ricerca vuoto (una foglia).
  *
  * @tparam E indica il tipo di elementi contenuti nell'BinarySearchTree. Deve essere ordinabile ed è invariante.
  */
private final case class Empty[E]() extends BinarySearchTree[E]