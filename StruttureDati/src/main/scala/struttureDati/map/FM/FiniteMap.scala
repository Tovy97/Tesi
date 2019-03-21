package struttureDati.map.FM

import scala.annotation.tailrec

/**
  * Implementa una mappa gestita come gli alberi binari di ricerca.
  * Una mappa è un insieme di coppie chiave-valore, nel quale ad ogni chiave è associato uno ed un solo valore.
  * La mappa crea un albero binario di ricerca, in cui la chiave, che deve essere ordinabile, è usata per strutturare l’albero.
  * Un albero binario di ricerca è un albero nel quale:
  *
  * 1) i due sottoalberi di un nodo sono a loro volta alberi binari di ricerca
  * 2) il sottoalbero sinistro di un nodo contiene soltanto i nodi con valori minori del nodo stesso
  * 3) il sottoalbero destro di un nodo contiene soltanto i nodi con valori maggiori del nodo stesso.
  *
  * Prevede due parametri, il primo per la chiave e il secondo per il valore.
  * Il parametro per la chiave è invariante e deve essere ordinabile, mentre quello per il valore è covariante.
  * La struttura dati è persistente.
  *
  * @tparam I indica il tipo della chiave della mappa. Deve essere ordinabile ed è invariante.
  * @tparam E indica il tipo del valore della mappa. È invariante.
  */
sealed trait FiniteMap[I, +E] {

  /**
    * Ritorna il numero di elementi presenti nella mappa.
    *
    * @return il numero di elementi presenti nella mappa.
    */
  final lazy val size: Int = this match {
    case Empty() => 0
    case Node(_, _, sx, dx) => sx.size + dx.size + 1
  }

  /**
    * Controlla se è presente almeno un elemento nella mappa.
    *
    * @return true se la mappa è vuota, altrimenti false
    */
  final lazy val isEmpty: Boolean = this match {
    case Empty() => true
    case Node(_, _, _, _) => false
  }

  /**
    * Ritorna la stringa che rappresenta la mappa.
    *
    * @return la stringa che rappresenta la mappa.
    */
  override final def toString: String = {
    def treeToString(fm: FiniteMap[I, E]): String = fm match {
      case Empty() => "."
      case Node(ind, el, sx, dx) => "(" + treeToString(sx) + "[" + ind + "]->[" + el + "]" + treeToString(dx) + ")"
    }

    "FiniteMap(" + treeToString(this) + ")"
  }

  /**
    * Inserisce una coppia chiave-valore nella mappa.
    * Se è già associato un valore a quella chiave, esso viene sostituito con il nuovo valore.
    * Complessità: O(n) nel caso peggiore
    *
    * @param Ind è la chiave associata al valore da inserire.
    * @param el  è il valore da inserire.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @tparam T è il tipo parametrico con cui viene parametrizzato il valore della mappa.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return la mappa con la coppia chiave-valore inserita.
    */
  @throws(classOf[IllegalArgumentException])
  final def insert[T >: E](Ind: I, el: T)(implicit ord: Ordering[I]): FiniteMap[I, T] = {
    require(!(ord eq null), "L'indice deve essere ordinabile")
    this match {
      case Empty() => Node(Ind, el, Empty(), Empty())
      case Node(Ind, _, sx, dx) => Node(Ind, el, sx, dx)
      case Node(i, e, sx, dx) => if (ord.lt(Ind, i)) {
        Node(i, e, sx.insert(Ind, el), dx)
      } else {
        Node(i, e, sx, dx.insert(Ind, el))
      }
    }
  }

  /**
    * Inserisce una coppia chiave-valore nella mappa.
    * Se è già associato un valore a quella chiave, esso viene sostituito con il nuovo valore.
    * Complessità: O(n) nel caso peggiore
    *
    * @param coppia è la coppia chiave-valore da inserire.
    * @param ord    è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @tparam T è il tipo parametrico con cui viene parametrizzato il valore della mappa.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return la mappa con la coppia chiave-valore inserita.
    */
  @throws(classOf[IllegalArgumentException])
  final def insert[T >: E](coppia: (I, T))(implicit ord: Ordering[I]): FiniteMap[I, T] = {
    require(!(ord eq null), "L'indice deve essere ordinabile")
    insert(coppia._1, coppia._2)
  }

  /**
    * Controlla se una chiave è usata nella mappa.
    * Complessità: O(n) nel caso peggiore
    *
    * @param Ind è la chiave da cercare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se la chiave Ind è usata nella mappa, altrimenti false
    */
  @tailrec
  @throws(classOf[IllegalArgumentException])
  final def isKeyUsed(Ind: I)(implicit ord: Ordering[I]): Boolean = {
    require(!(ord eq null), "L'indice deve essere ordinabile")
    this match {
      case Empty() => false
      case Node(Ind, _, _, _) => true
      case Node(i, _, sx, dx) => if (ord.lt(Ind, i)) {
        sx.isKeyUsed(Ind)
      } else {
        dx.isKeyUsed(Ind)
      }
    }
  }

  /**
    * Elimina la coppia chiave-valore con la chiave indicata dalla mappa.
    * Complessità: O(n) nel caso peggiore
    *
    * @param Ind è la chiave della coppia da eliminare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return la mappa in cui è stata eliminata la coppia chiave-valore
    */
  @throws(classOf[IllegalArgumentException])
  final def delete(Ind: I)(implicit ord: Ordering[I]): FiniteMap[I, E] = {
    require(!(ord eq null), "L'indice deve essere ordinabile")
    this match {
      case Empty() => Empty()
      case Node(Ind, _, Empty(), Empty()) => Empty()
      case Node(Ind, _, sx, Empty()) => sx
      case Node(Ind, _, Empty(), dx) => dx
      case Node(Ind, _, sx, dx) =>
        val (i, e): (I, E) = dx.getMin
        Node(i, e, sx, dx.delete(i))
      case Node(i, e, sx, dx) => if (ord.lt(Ind, i)) {
        Node(i, e, sx.delete(Ind), dx)
      } else {
        Node(i, e, sx, dx.delete(Ind))
      }
    }
  }

  /**
    * Ritorna il valore presente nella mappa associato alla chiave indicata.
    * Se la chiave cercata non è presenta nella mappa viene sollevata un'eccezione.
    * Complessità: O(n) nel caso peggiore
    *
    * @param Ind è la chiave associata al valore da cercare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @throws java.util.NoSuchElementException   se la chiave cercata non è presenta nella mappa
    * @return il valore associato alla chiave Ind.
    */
  @tailrec
  @throws(classOf[IllegalArgumentException])
  @throws(classOf[NoSuchElementException])
  final def getElement(Ind: I)(implicit ord: Ordering[I]): E = {
    require(!(ord eq null), "L'indice deve essere ordinabile")
    this match {
      case Empty() => throw new NoSuchElementException("Nessun elemento con l'indice cercato")
      case Node(Ind, e, _, _) => e
      case Node(i, _, sx, dx) => if (ord.lt(Ind, i)) {
        sx.getElement(Ind)
      } else {
        dx.getElement(Ind)
      }
    }
  }

  /**
    * Ritorna la coppia chiave-valore con la chiave maggiore presente nella mappa.
    * Se la mappa è vuota viene sollevata un'eccezione.
    * Complessità: O(n) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se la mappa è vuota
    * @return la coppia chiave-valore con la chiave maggior
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  final def getMax: (I, E) = this match {
    case Empty() => throw new NoSuchElementException("Empty().getMax")
    case Node(i, e, _, Empty()) => (i, e)
    case Node(_, _, _, dx) => dx.getMax
  }

  /**
    * Ritorna la coppia chiave-valore con la chiave minore presente nella mappa.
    * Se la mappa è vuota viene sollevata un'eccezione.
    * Complessità: O(n) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se la mappa è vuota
    * @return la coppia chiave-valore con la chiave minore
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  final def getMin: (I, E) = this match {
    case Empty() => throw new NoSuchElementException("Empty().getMax")
    case Node(i, e, Empty(), _) => (i, e)
    case Node(_, _, sx, _) => sx.getMin
  }

  /**
    * Controlla se le 3 proprietà degli alberi binari di ricerca usati per la rappresentazione
    * della mappa sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le 3 proprietà degli alberi binari di ricerca usati per la rappresentazione della mappa sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  final def isCorrect(implicit ord: Ordering[I]): Boolean = {
    require(!(ord eq null), "L'indice deve essere ordinabile")

    def check(bst: FiniteMap[I, E]): Boolean = bst match {
      case Empty() => true
      case Node(ind, _, sx, dx) => sx.toList.forall(x => ord.lt(x._1, ind)) && dx.toList.forall(x => ord.gt(x._1, ind)) && check(sx) && check(dx)
    }

    check(this)
  }

  /**
    * Ritorna la lista ordinata contenente le coppie chiave-valore degli elementi della mappa.
    *
    * @return la lista ordinata contenente le coppie chiave-valore degli elementi della mappa.
    */
  final def toList: List[(I, E)] = {
    def createList(rbt: FiniteMap[I, E], temp: List[(I, E)]): List[(I, E)] = rbt match {
      case Empty() => temp
      case Node(ind, el, sx, dx) => createList(sx, (ind, el) :: createList(dx, temp))
    }

    createList(this, Nil)
  }
}

/**
  * Companion Object del trait FiniteMap.
  * Permette la creazione di mappe che usano gli alberi binari di ricerca per
  * la rappresentazione.
  */
object FiniteMap {
  /**
    * Permette la creazione di mappe che usa un albero binario di ricerca per
    * la rappresentazione e che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nella mappa.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico per la chiave.
    * @tparam I è il tipo parametrico per la chiave con cui viene parametrizzata la mappa.
    * @tparam E è il tipo parametrico per il valore con cui viene parametrizzata la mappa.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return la mappa contenente gli elementi passati come parametro
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[I, E](els: (I, E)*)(implicit ord: Ordering[I]): FiniteMap[I, E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    if (els.isEmpty) {
      Empty()
    } else {
      els.foldLeft(FiniteMap[I, E]())((x, y) => x.insert(y._1, y._2))
    }
  }
}

/**
  * Rappresenta una mappa non vuota attraverso un albero binario di ricerca con una coppia
  * chiave-valore nella radice e due sottoalberi.
  *
  * @param ind è la chiave della coppia chiave-valore dell'elemento nella radice
  * @param el  è il valore della coppia chiave-valore dell'elemento nella radice
  * @param sx  è il sottoalbero sinistro del nodo radice.
  * @param dx  è il sottoalbero destro del nodo radice.
  * @tparam I indica il tipo della chiave della mappa. Deve essere ordinabile ed è invariante.
  * @tparam E indica il tipo del valore della mappa. È invariante.
  */
private final case class Node[I, +E](ind: I, el: E, sx: FiniteMap[I, E], dx: FiniteMap[I, E]) extends FiniteMap[I, E]

/**
  * Rappresenta una mappa vuota attraverso un albero binario di ricerca(una foglia).
  *
  * @tparam I indica il tipo della chiave della mappa. Deve essere ordinabile ed è invariante.
  * @tparam E indica il tipo del valore della mappa. È invariante.
  */
private final case class Empty[I, +E]() extends FiniteMap[I, E]