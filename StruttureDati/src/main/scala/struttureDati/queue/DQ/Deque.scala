package struttureDati.queue.DQ

import struttureDati.queue.Queue

/**
  * Implementa le code doppie, che sono code in cui le operazioni di inserimento,
  * lettura e cancellazione si possono compiere su entrambi gli estremi.
  *
  * @param sx è la parte iniziale della coda doppia (testa).
  * @param dx è a parte finale della coda doppia (coda).
  * @tparam E indica il tipo di elementi contenuti nella coda doppia. È covariante.
  */
final case class Deque[+E](private val sx: List[E], private val dx: List[E]) extends Queue[E] {

  /**
    * Ritorna il numero di elementi presenti nella coda.
    *
    * @return il numero di elementi presenti nella coda.
    */
  override lazy val size: Int = sx.size + dx.size

  /**
    * Controlla se è presente almeno un elemento della coda doppia.
    *
    * @return true se la coda doppia è vuota, altrimenti false
    */
  override lazy val isEmpty: Boolean = (sx, dx) match {
    case (Nil, Nil) => true
    case _ => false
  }

  /**
    * Inserisce un elemento in testa alla coda doppia.
    * Complessità: O(1) nel caso peggiore.
    * Complessità ammortizzata: O(1)
    *
    * @param el è l'elemento da inserire
    * @tparam T indica il tipo di elementi contenuti nella nuova coda doppia.
    * @return la coda doppia con il nuovo elemento inserito in testa.
    */
  override def addRight[T >: E](el: T): Deque[T] = check(sx, el :: dx)

  /**
    * Ritorna l'elemento che si trova in testa alla coda doppia.
    * Se la coda doppia è vuota viene sollevata un'eccezione.
    * Complessità: O(1) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se la coda doppia è vuota.
    * @return l'elemento che si trovava in testa alla coda doppia.
    */
  @throws(classOf[NoSuchElementException])
  override def head: E = (sx, dx) match {
    case (Nil, Nil) => throw new NoSuchElementException("Empty.head")
    case (Nil, h :: _) => h
    case (h :: _, _) => h
  }

  /**
    * Elimina l'elemento che si trova in testa alla coda doppia.
    * Complessità: O(n) nel caso peggiore
    * Complessità ammortizzata: O(1)
    *
    * @return la coda doppia senza l'elemento che si trovava in testa
    */
  override lazy val tail: Deque[E] = (sx, dx) match {
    case (Nil, Nil) => this
    case (Nil, _ :: _) => Deque(Nil, Nil)
    case (_ :: t, _) => check(t, dx)
  }

  /**
    * Ritorna la lista contenente gli elementi della coda doppia,
    * ordinati dalla testa alla coda.
    *
    * @return la lista contenente gli elementi della coda doppia.
    */
  override lazy val toList: List[E] = sx ++ dx.reverse

  /**
    * Controlla la seguente proprietà:
    *
    * - nessuna delle due liste deve essere vuota se vi sono almeno due elementi nella coda doppia.
    *
    * @return true se la proprietà dell'implementazione di Deque è rispettata, altrimenti false.
    */
  override lazy val isCorrect: Boolean = (sx, dx) match {
    case (_ :: _ :: _, Nil) => false
    case (Nil, _ :: _ :: _) => false
    case _ => true
  }

  /**
    * Ritorna la stringa che rappresenta la coda doppia.
    *
    * @return la stringa che rappresenta la coda doppia.
    */
  override def toString: String = {
    def listToString(l: List[E]): String = l match {
      case Nil => ""
      case h :: Nil => h.toString
      case h :: t => h + ", " + listToString(t)
    }

    (sx, dx) match {
      case (Nil, Nil) => "Deque()"
      case (Nil, _ :: _) => "Deque(" + listToString(dx.reverse) + ")"
      case (_ :: _, Nil) => "Deque(" + listToString(sx) + ")"
      case _ => "Deque(" + listToString(sx) + ", " + listToString(dx.reverse) + ")"
    }
  }

  /**
    * Ritorna l'elemento che si trova in coda alla coda doppia.
    * Se la coda doppia è vuota viene sollevata un'eccezione.
    * Complessità: O(1) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se la coda doppia è vuota.
    * @return l'elemento che si trovava in coda alla coda doppia.
    */
  @throws(classOf[NoSuchElementException])
  def last: E = (sx, dx) match {
    case (Nil, Nil) => throw new NoSuchElementException("Empty.last")
    case (h :: _, Nil) => h
    case (_, h :: _) => h
  }

  /**
    * Elimina l'elemento che si trova in coda alla coda doppia.
    * Complessità: O(n) nel caso peggiore
    * Complessità ammortizzata: O(1)
    *
    * @return la coda doppia senza l'elemento che si trovava in coda.
    */
  lazy val init: Deque[E] = (sx, dx) match {
    case (Nil, Nil) => this
    case (_ :: _, Nil) => Deque(Nil, Nil)
    case (_, _ :: t) => check(sx, t)
  }

  /**
    * Inserisce un elemento in coda alla coda doppia.
    * Complessità: O(1) nel caso peggiore.
    * Complessità ammortizzata: O(1).
    *
    * @param el è l'elemento da inserire
    * @tparam T indica il tipo di elementi contenuti nella nuova coda doppia.
    * @return la coda doppia con il nuovo elemento inserito in coda.
    */
  def addLeft[T >: E](el: T): Deque[T] = check(el :: sx, dx)

  /**
    * Bilancia la coda doppia spostando metà del contenuto di una lista in quell'altra,
    * nel caso in cui la seconda fosse vuota.
    * Prima dello spostamento di metà di una lista, la metà interessata viene
    * rovesciata (reverse).
    *
    * @param sinistra la lista sinistra (di testa) della coda doppia
    * @param destra   la lista destra (di coda) della coda doppia
    * @tparam T indica il tipo di elementi contenuti nella nuova coda doppia.
    * @return la nuova coda doppia bilanciata.
    */
  private def check[T >: E](sinistra: List[T], destra: List[T]): Deque[T] = (sinistra, destra) match {
    case (Nil, _ :: _ :: _) =>
      val (r, l) = destra.splitAt(destra.length / 2)
      Deque(l.reverse, r)
    case (_ :: _ :: _, Nil) =>
      val (l, r) = sinistra.splitAt(sinistra.length / 2)
      Deque(l, r.reverse)
    case _ => Deque(sinistra, destra)
  }
}

/**
  * Companion Object della classe Deque.
  * Permette la creazione di code doppie.
  */
object Deque {
  /**
    * Permette la creazione di una coda doppia che contiene gli elementi passati
    * come parametro.
    *
    * @param els l'elenco degli elementi da inserire nella coda doppia
    * @tparam E il tipo parametrico con cui viene parametrizzata la coda doppia
    * @return la coda doppia contenente gli elementi passati come parametro
    */
  final def apply[E](els: E*): Deque[E] = els.foldLeft(Deque[E](Nil, Nil))((x, y) => x addRight y)
}