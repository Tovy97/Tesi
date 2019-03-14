package struttureDati.orderedSet.RBT

import struttureDati.orderedSet.OrderedSet
import scala.annotation.tailrec

/**
  * Implementa gli alberi red-black.
  * Un albero red-black è un particolare albero binario di ricerca in cui:
  *
  * 1) ciascun nodo ha un attributo colore, il cui valore può essere rosso oppure nero
  * 2) il nodo radice è nero
  * 3) ogni foglia è nera ed è Empty (non contiene elementi)
  * 4) entrambi i figli di ciascun nodo rosso sono neri
  * 5) ogni cammino dalla radice ad una foglia contiene lo stesso numero di nodi neri.
  * 6) quando si crea un nuovo nodo, prima di aggiungerlo all'albero, lo si colora di rosso.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  * La struttura dati è persistente.
  *
  * @tparam E indica il tipo di elementi contenuti nell'RedBlackTree. Deve essere ordinabile ed è invariante.
  */
sealed trait RedBlackTree[E] extends OrderedSet[E] {

  /**
    * Rappresenta il colore del nodo.
    *
    * @return il colore del nodo
    */
  protected def col: Color

  /**
    * Ritorna il numero di elementi presenti nell'albero red-black.
    *
    * @return il numero di elementi presenti nell'albero red-black.
    */
  override final lazy val size: Int = this match {
    case Empty() => 0
    case Node(_, _, sx, dx) => sx.size + dx.size + 1
  }

  /**
    * Controlla se è presente almeno un elemento nell'albero red-black.
    *
    * @return true se l'albero red-black è vuoto, altrimenti false
    */
  override final lazy val isEmpty: Boolean = this match {
    case Empty() => true
    case Node(_, _, _, _) => false
  }

  /**
    * Inserisce un elemento nell'albero red-black.
    * Se l'elemento è già presente, non viene aggiunto.
    * Complessità: O(log(n)) nel caso peggiore
    *
    * @param El  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'albero red-black in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  override final def insert(El: E)(implicit ord: Ordering[E]): RedBlackTree[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def ins(rbt: RedBlackTree[E]): Node[E] = rbt match {
      case Empty() => Node(Red, El, Empty(), Empty())
      case Node(col, El, a, b) => Node(col, El, a, b)
      case Node(col, y, a, b) => if (ord.lt(El, y)) {
        balance(col, y, ins(a), b)
      } else {
        balance(col, y, a, ins(b))
      }
    }

    val temp = ins(this)
    Node(Black, temp.el, temp.sx, temp.dx)
  }

  /**
    * Controlla se un elemento è presente nell'albero red-black.
    * Complessità: O(log(n)) nel caso peggiore
    *
    * @param El  è l'elemento da cercare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se El è contenuto nell'albero red-black, altrimenti false
    */
  @tailrec
  @throws(classOf[IllegalArgumentException])
  override final def isMember(El: E)(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    this match {
      case Empty() => false
      case Node(_, El, _, _) => true
      case Node(_, e, a, b) => if (ord.lt(El, e)) {
        a.isMember(El)
      } else {
        b.isMember(El)
      }
    }
  }

  /**
    * Elimina un elemento dall'albero red-black.
    * Complessità: O(log(n)) nel caso peggiore
    *
    * @param El  è l'elemento da eliminare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'albero red-black in cui è stato eliminato l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  override final def delete(El: E)(implicit ord: Ordering[E]): RedBlackTree[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def balLeft(rbt1: RedBlackTree[E], e: E, rbt2: RedBlackTree[E]): RedBlackTree[E] = (rbt1, e, rbt2) match {
      case (Node(Red, x, a, b), y, c) => Node(Red, y, Node(Black, x, a, b), c)
      case (bl, x, Node(Black, y, a, b)) => balance(Black, x, bl, Node(Red, y, a, b))
      case (bl, x, Node(Red, z, Node(Black, y, a, b), Node(Black, k, r, t))) => Node(Red, y, Node(Black, x, bl, a), balance(Black, z, b, Node(Red, k, r, t)))
      case _ => Node(Black, e, rbt1, rbt2)
    }

    def balRight(rbt1: RedBlackTree[E], e: E, rbt2: RedBlackTree[E]): RedBlackTree[E] = (rbt1, e, rbt2) match {
      case (a, x, Node(Red, y, b, c)) => Node(Red, x, a, Node(Black, y, b, c))
      case (Node(Black, x, a, b), y, bl) => balance(Black, y, Node(Red, x, a, b), bl)
      case (Node(Red, x, Node(Black, k, r, t), Node(Black, y, b, c)), z, bl) => Node(Red, y, balance(Black, x, Node(Red, k, r, t), b), Node(Black, z, c, bl))
      case _ => Node(Black, e, rbt1, rbt2)
    }

    def delRight(a: RedBlackTree[E], y: E, b: RedBlackTree[E]): RedBlackTree[E] = b match {
      case Node(Black, _, _, _) => balRight(a, y, del(b))
      case _ => Node(Red, y, a, del(b))
    }

    def delLeft(a: RedBlackTree[E], y: E, b: RedBlackTree[E]): RedBlackTree[E] = a match {
      case Node(Black, _, _, _) => balLeft(del(a), y, b)
      case _ => Node(Red, y, del(a), b)
    }

    def merge(rbt1: RedBlackTree[E], rbt2: RedBlackTree[E]): RedBlackTree[E] = (rbt1, rbt2) match {
      case (Empty(), x) => x
      case (x, Empty()) => x
      case (Node(Red, x, a, b), Node(Red, y, c, d)) =>
        val k = merge(b, c)
        k match {
          case Node(Red, z, b1, c1) => Node(Red, z, Node(Red, x, a, b1), Node(Red, y, c1, d))
          case _ => Node(Red, x, a, Node(Red, y, k, d))
        }
      case (Node(Black, x, a, b), Node(Black, y, c, d)) =>
        val k = merge(b, c)
        k match {
          case Node(Red, z, b1, c1) => Node(Red, z, Node(Black, x, a, b1), Node(Black, y, c1, d))
          case _ => balLeft(a, x, Node(Black, y, k, d))
        }
      case (a, Node(Red, x, b, c)) => Node(Red, x, merge(a, b), c)
      case (Node(Red, x, a, b), c) => Node(Red, x, a, merge(b, c))
    }

    def del(rbt: RedBlackTree[E]): RedBlackTree[E] = rbt match {
      case Node(_, El, a, b) => merge(a, b)
      case Node(_, e, a, b) => if (ord.lt(El, e)) {
        delLeft(a, e, b)
      } else {
        delRight(a, e, b)
      }
      case Empty() => rbt
    }

    del(this) match {
      case Empty() => Empty()
      case Node(_, e, sx, dx) => Node(Black, e, sx, dx)
    }
  }

  /**
    * Ritorna l'elemento minore presente nell'albero red-black.
    * Se l'albero red-black è vuoto viene sollevata un'eccezione.
    * Complessità: O(log(n)) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se l'albero red-black è vuoto
    * @return l'elemento minore presente nell'albero red-black.
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  override final def getMin: E = this match {
    case Empty() => throw new NoSuchElementException("Empty.getMin")
    case Node(_, e, Empty(), _) => e
    case Node(_, _, sx, _) => sx.getMin
  }

  /**
    * Ritorna l'elemento maggiore presente nell'albero red-black.
    * Se l'albero red-black è vuoto viene sollevata un'eccezione.
    * Complessità: O(log(n)) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se l'albero red-black è vuoto
    * @return l'elemento maggiore presente nell'albero red-black.
    */
  @tailrec
  @throws(classOf[NoSuchElementException])
  override final def getMax: E = this match {
    case Empty() => throw new NoSuchElementException("Empty().getMax")
    case Node(_, e, _, Empty()) => e
    case Node(_, _, _, dx) => dx.getMax
  }

  /**
    * Controlla se le 6 proprietà degli alberi red-black sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le 6 proprietà degli alberi red-black sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  override final def isCorrect(implicit ord: Ordering[E]): Boolean = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")

    def checkOrder(rbt: RedBlackTree[E]): Boolean = rbt match {
      case Empty() => true
      case Node(_, el, sx, dx) => sx.toList.forall(x => ord.lt(x, el)) && dx.toList.forall(x => ord.gt(x, el)) && checkOrder(sx) && checkOrder(dx)
    }

    def checkRed(rbt: RedBlackTree[E]): Boolean = rbt match {
      case Empty() => true
      case Node(Red, _, sx, dx) => (sx, dx) match {
        case (Empty(), Empty()) => true
        case (Node(colSx, _, _, _), Empty()) => colSx == Black && checkRed(sx)
        case (Empty(), Node(colDx, _, _, _)) => colDx == Black && checkRed(dx)
        case (Node(colSx, _, _, _), Node(colDx, _, _, _)) => colSx == Black && colDx == Black && checkRed(sx) && checkRed(dx)
      }
      case Node(Black, _, sx, dx) => checkRed(sx) && checkRed(dx)
    }

    def checkBlack(rbt: RedBlackTree[E]): (Boolean, Int) = rbt match {
      case Empty() => (true, 0)
      case Node(col, _, sx, dx) =>
        val checkSx = checkBlack(sx)
        val checkDx = checkBlack(dx)
        if (checkSx._1 && checkDx._1 && checkSx._2 == checkDx._2) {
          col match {
            case Red => (true, checkSx._2)
            case Black => (true, checkSx._2 + 1)
          }
        } else {
          (false, -1)
        }
    }

    this.col == Black && checkOrder(this) && checkRed(this) && checkBlack(this)._1
  }

  /**
    * Ritorna la lista ordinata contenente gli elementi dell'albero red-black.
    * Complessità: O(n) nel caso peggiore
    *
    * @return la lista ordinata contenente gli elementi dell'albero red-black.
    */
  override final lazy val toList: List[E] = {
    def createList(rbt: RedBlackTree[E], temp: List[E]): List[E] = rbt match {
      case Empty() => temp
      case Node(_, el, sx, dx) => createList(sx, el :: createList(dx, temp))
    }

    createList(this, Nil)
  }

  /**
    * Ritorna la stringa che rappresenta l'albero red-black.
    *
    * @return la stringa che rappresenta l'albero red-black.
    */
  override final lazy val toString: String = {
    def treeToString(rbt: RedBlackTree[E]): String = rbt match {
      case Empty() => "."
      case Node(Red, el, sx, dx) => "(" + treeToString(sx) + el + treeToString(dx) + ")"
      case Node(Black, el, sx, dx) => "[" + treeToString(sx) + el + treeToString(dx) + "]"
    }

    "RedBlackTree(" + treeToString(this) + ")"
  }

  /**
    * Bilancia l'albero red-black, al fine di garantire che la proprietà 4 sia
    * sempre rispettata. Se l'albero non viola la proprietà 4, allora non subisce modifiche.
    *
    * @param col  è il colore della radice del sottoalbero red-black che si sta bilanciando
    * @param el   è l'elemento della radice del sottoalbero red-black che si sta bilanciando
    * @param rbt1 è il sottoalbero sinistro della radice del sottoalbero red-black che si sta bilanciando
    * @param rbt2 è il sottoalbero destro della radice del sottoalbero red-black che si sta bilanciando
    * @return l'albero red-black bilanciato.
    */
  private final def balance(col: Color, el: E, rbt1: RedBlackTree[E], rbt2: RedBlackTree[E]): Node[E] = {
    (col, el, rbt1, rbt2) match {
      case (Black, z, Node(Red, y, Node(Red, x, a, b), c), d) => Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, z, Node(Red, x, a, Node(Red, y, b, c)), d) => Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) => Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, x, a, Node(Red, z, Node(Red, y, b, c), d)) => Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case _ => Node(col, el, rbt1, rbt2)
    }
  }
}

/**
  * Companion Object del trait RedBlackTree.
  * Permette la creazione di alberi red-black.
  */
object RedBlackTree {
  /**
    * Permette la creazione di un'albero red-black che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nell'abero red-black.
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @tparam E è il tipo parametrico con cui viene parametrizzato l'albero red-black.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'albero red-black contenente gli elementi passati come parametro
    */
  @throws(classOf[IllegalArgumentException])
  final def apply[E](els: E*)(implicit ord: Ordering[E]): RedBlackTree[E] = {
    require(!(ord eq null), "Il tipo deve essere ordinabile")
    if (els.isEmpty) {
      Empty()
    } else {
      els.foldLeft(RedBlackTree())((x, y) => x insert y)
    }
  }
}

/**
  * Rappresenta un albero red-black non vuoto con un elemento nella radice e due sottoalberi.
  *
  * @param col è il colore del nodo radice.
  * @param el  è l'elemento contenuto nel nodo radice.
  * @param sx  è il sottoalbero red-black sinistro del nodo radice.
  * @param dx  è il sottoalbero red-black destro del nodo radice.
  * @tparam E indica il tipo di elementi contenuti nell'RedBlackTree. Deve essere ordinabile ed è invariante.
  */
private final case class Node[E](col: Color, el: E, sx: RedBlackTree[E], dx: RedBlackTree[E]) extends RedBlackTree[E]

/**
  * Rappresenta un albero red-black vuoto (una foglia).
  *
  * @tparam E indica il tipo di elementi contenuti nell'RedBlackTree. Deve essere ordinabile ed è invariante.
  */
private final case class Empty[E]() extends RedBlackTree[E] {
  /**
    * Rappresenta il colore della foglia, che per la proprietà 3 è sempre nera.
    */
  override val col: Color = Black
}