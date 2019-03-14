package struttureDati.minHeap

/**
  * Rappresenta un min-heap, ovvero una struttura dati basata sugli alberi che soddisfa
  * la seguente proprietà:
  *
  * 1) se P è un nodo genitore di C, il valore di P è minore o uguale al valore di C.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  *
  * @tparam E indica il tipo di elementi contenuti nel MinHeap. Deve essere ordinabile ed è invariante.
  */
trait MinHeap[E] {

  /**
    * Ritorna il numero di elementi presenti nell'heap.
    *
    * @return il numero di elementi presenti nell'heap.
    */
  def size: Int

  /**
    * Controlla se è presente almeno un elemento nell'heap.
    *
    * @return true se l'heap è vuoto, altrimenti false
    */
  def isEmpty: Boolean

  /**
    * Inserisce un elemento nell'heap.
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'heap in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  def insert(el: E)(implicit ord: Ordering[E]): MinHeap[E]

  /**
    * Unisce gli elementi presenti in due heap in un unico heap.
    *
    * @param hp  è l'heap da unire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @throws java.lang.IllegalArgumentException se l'heap hp passato come parametro non è compatibile
    * @return l'heap unione
    */
  @throws(classOf[IllegalArgumentException])
  def merge(hp: MinHeap[E])(implicit ord: Ordering[E]): MinHeap[E]

  /**
    * Ritorna l'elemento minore presente nell'heap.
    * Se l'heap è vuoto viene sollevata un'eccezione.
    *
    * @throws java.util.NoSuchElementException se l'heap è vuoto.
    * @return l'elemento minore presente nell'heap.
    */
  @throws(classOf[NoSuchElementException])
  def findMin: E

  /**
    * Elimina l'elemento minore presente nell'heap.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'heap in cui è stato eliminato l'elemento minore.
    */
  @throws(classOf[IllegalArgumentException])
  def deleteMin(implicit ord: Ordering[E]): MinHeap[E]

  /**
    * Controlla se le proprietà dell'implementazione del MinHeap sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le proprietà dell'implementazione del MinHeap sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  def isCorrect(implicit ord: Ordering[E]): Boolean

  /**
    * Ritorna la lista contenente gli elementi del MinHeap.
    *
    * @return la lista contenente gli elementi del MinHeap.
    */
  def toList: List[E]
}

