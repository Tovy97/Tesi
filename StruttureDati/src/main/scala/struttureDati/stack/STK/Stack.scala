package struttureDati.stack.STK

/**
  * Implementa le pile, che sono strutture dati LIFO.
  * Prevede un tipo parametrico covariante.
  * La struttura dati è persistente.
  *
  * @param stack è la pila di elementi che contiene lo stack
  * @tparam E indica il tipo di elementi contenuti nello Stack. È covariante.
  */
final case class Stack[+E](private val stack: List[E]) {

  /**
    * Ritorna il numero di elementi presenti nell'heap.
    *
    * @return il numero di elementi presenti nell'heap.
    */
  lazy val size: Int = stack.size

  /**
    * Elimina l'elemento che si trova in cima alla pila.
    * Se la pila è vuota solleva un'eccezione.
    * Complessità: O(1) nel caso peggiore.
    *
    * @return lo stack senza l'elemento che si trovava in cima.
    */
  def pop: Stack[E] = stack match {
    case Nil => this
    case _ :: t => Stack(t)
  }

  /**
    * Aggiunge un elemento in cima allo stack.
    * Complessità: O(1) nel caso peggiore
    *
    * @param el è l'elemento da aggiungere alla pila
    * @tparam T indica il tipo di elementi contenuti nel nuovo Stack.
    * @return lo stack con il nuovo elemento in cima
    */
  def push[T >: E](el: T): Stack[T] = {
    Stack(el :: stack)
  }

  /**
    * Ritorna l'elemento che si trova in cima alla pila.
    * Se la pila è vuota solleva un'eccezione.
    * Complessità: O(1) nel caso peggiore
    *
    * @return l'elemento che si trovava in cima allo stack.
    * @throws NoSuchElementException se non ci sono elementi nella pila
    */
  @throws(classOf[NoSuchElementException])
  def top: E = stack match {
    case Nil => throw new NoSuchElementException("Empty.top")
    case h :: _ => h
  }

  /**
    * Controlla se è presente almeno un elemento nello stack.
    *
    * @return true se la pila è vuota, altrimenti false
    */
  lazy val isEmpty: Boolean = stack match {
    case Nil => true
    case _ :: _ => false
  }

  /**
    * Ritorna la lista contenente gli elementi dello stack.
    *
    * @return la lista contenente gli elementi dello stack.
    */
  lazy val toList: List[E] = stack

  /**
    * Ritorna la stringa che rappresenta la pila.
    *
    * @return la stringa che rappresenta la pila.
    */
  override def toString: String = {
    def listToString(l: List[E]): String = l match {
      case Nil => ""
      case h :: Nil => h.toString
      case h :: t => h + ", " + listToString(t)
    }

    "Stack(" + listToString(stack) + ")"
  }
}

/**
  * Companion Object della classe Stack.
  * Permette la creazione di stack.
  */
object Stack {
  /**
    * Permette la creazione di uno stack che contiene gli elementi passati come parametro.
    *
    * @param els l'elenco degli elementi da inserire nello stack.
    * @tparam E è il tipo parametrico con cui viene parametrizzato lo Stack.
    * @return la pila contenente gli elementi passati come parametro
    */
  final def apply[E](els: E*): Stack[E] = new Stack(els.toList)
}
