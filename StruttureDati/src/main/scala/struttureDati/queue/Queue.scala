package struttureDati.queue

/**
  * Rappresenta rappresenta le code che sono un particolare tipo di lista
  * in cui le operazioni di inserimento, lettura e di cancellazione si compiono
  * sempre e solo agli estremi (testa e/o coda).
  * Prevede un parametro covariante.
  *
  * @tparam E indica il tipo di elementi contenuti nella coda. È covariante.
  */
trait Queue[+E] {

  /**
    * Ritorna il numero di elementi presenti nella coda.
    *
    * @return il numero di elementi presenti nella coda.
    */
  def size: Int

  /**
    * Controlla se è presente almeno un elemento della coda.
    *
    * @return true se la coda è vuota, altrimenti false
    */
  def isEmpty: Boolean

  /**
    * Inserisce un elemento in coda.
    *
    * @param el è l'elemento da inserire
    * @tparam T indica il tipo di elementi contenuti nella nuova coda.
    * @return la coda con il nuovo elemento inserito
    */
  def addRight[T >: E](el: T): Queue[T]

  /**
    * Ritorna l'elemento che si trova in testa alla coda.
    * Se la coda è vuota viene sollevata un'eccezione.
    *
    * @throws java.util.NoSuchElementException se la coda è vuota.
    * @return l'elemento che si trovava in testa alla coda.
    */
  @throws(classOf[NoSuchElementException])
  def head: E

  /**
    * Elimina l'elemento che si trova in testa alla coda.
    *
    * @return la coda senza l'elemento che si trovava in testa
    */
  def tail: Queue[E]

  /**
    * Ritorna la lista contenente gli elementi della coda.
    *
    * @return la lista contenente gli elementi della coda.
    */
  def toList: List[E]
}
