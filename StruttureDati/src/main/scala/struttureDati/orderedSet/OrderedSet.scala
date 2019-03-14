package struttureDati.orderedSet

/**
  * Rappresenta gli insieme di elementi ordinabili.
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  *
  * @tparam E indica il tipo di elementi contenuti nell'OrderedSet. Deve essere ordinabile ed è invariante.
  */
trait OrderedSet[E] {

  /**
    * Ritorna il numero di elementi presenti nell'insieme.
    *
    * @return il numero di elementi presenti nell'insieme.
    */
  def size: Int

  /**
    * Controlla se è presente almeno un elemento nell'insieme.
    *
    * @return true se l'insieme è vuoto, altrimenti false
    */
  def isEmpty: Boolean

  /**
    * Inserisce un elemento nell'insieme.
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'insieme in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  def insert(el: E)(implicit ord: Ordering[E]): OrderedSet[E]

  /**
    * Controlla se un elemento è presente nell'insieme.
    *
    * @param el  è l'elemento da cercare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se El è contenuto nell'insieme, altrimenti false
    */
  @throws(classOf[IllegalArgumentException])
  def isMember(el: E)(implicit ord: Ordering[E]): Boolean

  /**
    * Elimina un elemento dall'insieme.
    *
    * @param el  è l'elemento da eliminare
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return l'insieme in cui è stato eliminato l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  def delete(el: E)(implicit ord: Ordering[E]): OrderedSet[E]

  /**
    * Ritorna l'elemento minore presente nell'insieme.
    * Se l'insieme è vuoto viene sollevata un'eccezione.
    *
    * @throws java.util.NoSuchElementException se l'insieme è vuoto.
    * @return l'elemento minore presente nell'insieme.
    */
  @throws(classOf[NoSuchElementException])
  def getMin: E

  /**
    * Ritorna l'elemento maggiore presente nell'insieme.
    * Se l'insieme è vuoto viene sollevata un'eccezione.
    *
    * @throws java.util.NoSuchElementException se l'insieme è vuoto.
    * @return l'elemento maggiore presente nell'insieme.
    */
  @throws(classOf[NoSuchElementException])
  def getMax: E

  /**
    * Controlla se le proprietà dell'implementazione dell'OrderedSet sono rispettate.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return true se le proprietà dell'implementazione dell'OrderedSet sono rispettate, altrimenti false.
    */
  @throws(classOf[IllegalArgumentException])
  def isCorrect(implicit ord: Ordering[E]): Boolean

  /**
    * Ritorna la lista ordinata contenente gli elementi dell'OrderedSet.
    *
    * @return la lista ordinata contenente gli elementi dell'OrderedSet.
    */
  def toList: List[E]
}
