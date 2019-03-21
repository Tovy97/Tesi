package struttureDati.minHeap.BH

import scala.annotation.tailrec

/**
  * Implementa gli alberi binomiali che sono alberi n-ari definiti induttivamente sul grado come segue:
  *
  * base: un albero binomiale di grado 0 è un nodo con un elemento e senza figli.
  *
  * induzione: un albero binomiale di grado R+1 è formato collegando due alberi binomiali di grado R,
  * rendendo un albero il figlio più a sinistra dell'altro (ovvero collegati assieme in modo che la
  * radice di uno dei due alberi binomiali sia figlio sinistro della radice dell'altro).
  *
  * Quindi un albero binomiale di grado R ha esattamente 2 elevato R elementi.
  *
  * @param el       è l'elemento contenuto nella radice dell'albero binomiale.
  * @param rank     è il grado dell'albero binomiale
  * @param children è l'insieme dei figli del nodo radice, che sono a loro volta alberi binomiali
  * @tparam E indica il tipo di elementi contenuti nel BinomialTree. Deve essere ordinabile ed è invariante.
  * @throws java.lang.IllegalArgumentException se il parametro ord è null
  * @throws java.lang.IllegalArgumentException se l'albero binomiale creato non è corretto
  */
@throws(classOf[IllegalArgumentException])
@throws(classOf[IllegalArgumentException])
final case class BinomialTree[E](el: E, rank: Int, children: List[BinomialTree[E]])(implicit ord: Ordering[E]) {
  require(!(ord eq null), "Il tipo deve essere ordinabile")
  require(isCorrect, "L'albero binomiale creato non è corretto!")

  /**
    * Ritorna il numero di elementi presenti nell'albero binomiale.
    *
    * @return il numero di elementi presenti nell'albero binomiale.
    */
  lazy val size: Int = children match {
    case Nil => 1
    case _ => children.map(x => x.size).sum + 1
  }

  /**
    * Controlla se la proprietà degli degli alberi binomiali è rispettata.
    *
    * @return true se la proprietà degli degli alberi binomiali è rispettata, altrimenti false.
    */
  lazy val isCorrect: Boolean = this match {
    case BinomialTree(_, r, Nil) => r == 0
    case BinomialTree(_, r, c) =>
      @tailrec
      def isRankCorrect(i: Int): Boolean = {
        if (i - 1 < c.length) {
          r - i == c(i - 1).rank && isRankCorrect(i + 1)
        } else {
          true
        }
      }

      isRankCorrect(1) && c.forall(x => x.isCorrect) && c.forall(x => ord.lteq(el, x.el)) && size == Math.pow(2, rank)
  }

  /**
    * Ritorna una lista contenente gli elementi dell'albero binomiale scorrendo i figli di un nodo da
    * sinistra a destra e aggiungendo in testa il nodo stesso.
    *
    * @return la lista contenente gli elementi dell'albero binomiale scorrendo i figli di un nodo da sinistra a destra e aggiungendo in testa il nodo stesso.
    */
  lazy val toList: List[E] = {

    def treeToList(c: List[BinomialTree[E]]): List[E] = c match {
      case Nil => Nil
      case _ => c.flatMap(x => x.toList)
    }

    el :: treeToList(children)
  }

  /**
    * Ritorna la stringa che rappresenta l'albero binomiale.
    *
    * @return la stringa che rappresenta l'albero binomiale.
    */
  override lazy val toString: String = {
    def listToString(l: List[BinomialTree[E]]): String = l match {
      case Nil => ""
      case h :: Nil => h.toString
      case h :: t => h + ", " + listToString(t)
    }

    children match {
      case Nil => "BinomialTree(" + el + ")"
      case _ => "BinomialTree(" + el + ", (" + listToString(children) + "))"
    }
  }
}