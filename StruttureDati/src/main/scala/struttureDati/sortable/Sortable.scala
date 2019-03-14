package struttureDati.sortable

/**
  * Rappresenta una collezione di dati ordinabili che devono essere trasformati in una lista
  * ordinata.
  *
  * Prevede un parametro invariante.
  * Il tipo parametrico deve essere ordinabile.
  *
  * @tparam E indica il tipo di elementi contenuti nel Sortable. Deve essere ordinabile ed è invariante.
  */
trait Sortable[E] {

  /**
    * Ritorna il numero di elementi presenti nella collezione.
    *
    * @return il numero di elementi presenti nella collezione.
    */
  def size: Int

  /**
    * Controlla se è presente almeno un elemento nella collezione.
    *
    * @return true se la collezione è vuota, altrimenti false
    */
  def isEmpty: Boolean

  /**
    * Inserisce un elemento nella collezione.
    *
    * @param el  è l'elemento da inserire
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return la collezione in cui è stato inserito l'elemento
    */
  @throws(classOf[IllegalArgumentException])
  def add(el: E)(implicit ord: Ordering[E]): Sortable[E]

  /**
    * Ordina la collezione e restituisce la lista ordinata con gli elementi della collezione.
    *
    * @param ord è la classe contenente il criterio di ordinamento del tipo parametrico.
    * @throws java.lang.IllegalArgumentException se il parametro ord è null
    * @return la lista ordinata con gli elementi della collezione.
    */
  @throws(classOf[IllegalArgumentException])
  def sort(implicit ord: Ordering[E]): List[E]

  /**
    * Controlla se le proprietà della collezione sono rispettate.
    *
    * @return true se le proprietà della collezione sono rispettate, altrimenti false.
    */
  def isCorrect: Boolean
}
