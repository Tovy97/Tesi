package struttureDati.queue.BQ

import struttureDati.queue.Queue

/**
  * Implementa le code FIFO, che sono code in cui le operazioni di inserimento
  * si compiono sempre e solo in coda, mentre le operazioni di lettura e cancellazione
  * si compiono sempre e solo in testa.
  *
  * @param sx è la parte iniziale della coda (testa), da cui leggere ed eliminare gli elementi.
  * @param dx è a parte finale della coda (coda), in cui inserire gli elementi nuovi.
  * @tparam E indica il tipo di elementi contenuti nella coda. È covariante.
  * @throws java.lang.IllegalArgumentException se la coda creata non è corretta
  */
@throws(classOf[IllegalArgumentException])
final case class BatchedQueue[+E](private val sx: List[E], private val dx: List[E]) extends Queue[E] {
  require(isCorrect, "La coda creata non è corretta!")

  /**
    * Ritorna il numero di elementi presenti nella coda.
    *
    * @return il numero di elementi presenti nella coda.
    */
  override lazy val size: Int = sx.size + dx.size

  /**
    * Controlla se è presente almeno un elemento della coda.
    *
    * @return true se la coda è vuota, altrimenti false
    */
  override lazy val isEmpty: Boolean = (sx, dx) match {
    case (Nil, Nil) => true
    case _ => false
  }

  /**
    * Elimina l'elemento che si trova in testa alla coda.
    * Complessità: O(n) nel caso peggiore
    * Complessità ammortizzata: O(1)
    *
    * @return la coda senza l'elemento che si trovava in testa
    */
  override lazy val tail: BatchedQueue[E] = sx match {
    case Nil => this
    case _ :: t => checkLeft(t, dx)
  }

  /**
    * Ritorna la lista contenente gli elementi della coda, ordinati dalla testa alla coda.
    *
    * @return la lista contenente gli elementi della coda.
    */
  override lazy val toList: List[E] = sx ++ dx.reverse

  /**
    * Controlla la seguente proprietà:
    *
    * - la prima lista è vuota solo se lo è anche la seconda, ma non viceversa.
    *
    * @return true se la proprietà dell'implementazione di BatchedQueue è rispettata, altrimenti false.
    */
  override lazy val isCorrect: Boolean = (sx, dx) match {
    case (Nil, _ :: _) => false
    case _ => true
  }

  /**
    * Inserisce un elemento in coda.
    * Complessità: O(1) nel caso peggiore
    *
    * @param el è l'elemento da inserire
    * @tparam T indica il tipo di elementi contenuti nella nuova coda.
    * @return la coda con il nuovo elemento inserito
    */
  override def addRight[T >: E](el: T): BatchedQueue[T] = checkLeft(sx, el :: dx)

  /**
    * Bilancia la coda spostando il contenuto della lista di destra in quella di
    * sinistra nel caso in cui quest'ultima fosse vuota.
    * Prima dello spostamento della lista destra in quella sinistra, la
    * lista di destra viene rovesciata (reverse).
    *
    * @param sinistra la lista sinistra (di testa) della coda
    * @param destra   la lista destra (di coda) della coda
    * @tparam T indica il tipo di elementi contenuti nella nuova coda.
    * @return la nuova coda bilanciata
    */
  private def checkLeft[T >: E](sinistra: List[T], destra: List[T]): BatchedQueue[T] = (sinistra, destra) match {
    case (Nil, r) => BatchedQueue(r.reverse, Nil)
    case _ => BatchedQueue(sinistra, destra)
  }

  /**
    * Ritorna l'elemento che si trova in testa alla coda.
    * Se la coda è vuota viene sollevata un'eccezione.
    * Complessità: O(1) nel caso peggiore
    *
    * @throws java.util.NoSuchElementException se la coda è vuota.
    * @return l'elemento che si trovava in testa alla coda.
    */
  @throws(classOf[NoSuchElementException])
  override def head: E = sx match {
    case Nil => throw new NoSuchElementException("Empty.head")
    case h :: _ => h
  }

  /**
    * Ritorna la stringa che rappresenta la coda.
    *
    * @return la stringa che rappresenta la coda.
    */
  override def toString: String = {
    def listToString(l: List[E]): String = l match {
      case Nil => ""
      case h :: Nil => h.toString
      case h :: t => h + ", " + listToString(t)
    }

    (sx, dx) match {
      case (Nil, Nil) => "BatchedQueue()"
      case (Nil, _ :: _) => "BatchedQueue(" + listToString(dx.reverse) + ")"
      case (_ :: _, Nil) => "BatchedQueue(" + listToString(sx) + ")"
      case _ => "BatchedQueue(" + listToString(sx) + ", " + listToString(dx.reverse) + ")"
    }
  }
}

/**
  * Companion Object della classe BatchedQueue.
  * Permette la creazione di code.
  */
object BatchedQueue {
  /**
    * Permette la creazione di una coda che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nella coda
    * @tparam E il tipo parametrico con cui viene parametrizzata la coda
    * @return la coda contenente gli elementi passati come parametro
    */
  final def apply[E](els: E*): BatchedQueue[E] = els.foldLeft(BatchedQueue[E](Nil, Nil))((x, y) => x addRight y)
}