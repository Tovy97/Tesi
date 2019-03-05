package specificheStruttureDati

import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.orderedSet.RBT.RedBlackTree

/**
  * Implementa le specifiche di RBT, con tutte le sue proprietà da verificare.
  */
private object RBT_Specification extends Properties("RBT") {

  /**
    * Genera BST attraverso il metodo apply del Companion Object
    */
  private val genTree: Gen[RedBlackTree[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield RedBlackTree(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati RBT e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni RBT creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    rbt.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un RBT genera un RBT corretto
    */
  property("insert.isCorrect") = forAll(genTree, Arbitrary.arbitrary[Int]) { (rbt: RedBlackTree[Int], e: Int) =>
    rbt.insert(e).isCorrect
  }

  /**
    * Proprietà: la delete di un elemento E in un RBT genera un RBT corretto
    */
  property("delete.isCorrect") = forAll(genTree, Arbitrary.arbitrary[Int]) { (rbt: RedBlackTree[Int], e: Int) =>
    rbt.delete(e).isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un RBT aumenta di 1 la size del RBT se il RBT non
    * contiene già quell'elemento (ovvero se la isMember ritorna false), altrimenti la lascia
    * invariata.
    */
  property("insert.size") = forAll(genTree, Arbitrary.arbitrary[Int]) { (rbt: RedBlackTree[Int], e: Int) =>
    if (rbt.isMember(e)) {
      rbt.insert(e).size == rbt.size
    } else {
      rbt.insert(e).size == rbt.size + 1
    }
  }

  /**
    * Proprietà: la delete di un elemento E in un RBT diminuisce di 1 la size del RBT se il
    * RBT contiene quell'elemento (ovvero se la isMember ritorna true), altrimenti la lascia
    * invariata.
    */
  property("delete.size") = forAll(genTree, Arbitrary.arbitrary[Int]) { (rbt: RedBlackTree[Int], e: Int) =>
    if (rbt.isMember(e)) {
      rbt.delete(e).size == rbt.size - 1
    } else {
      rbt.delete(e).size == rbt.size
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa del RBT
    */
  property("toList.size") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    rbt.toList.size == rbt.size
  }

  /**
    * Proprietà: la toList genera una lista senza elementi ripetuti
    */
  property("toList.distinct") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    val l = rbt.toList
    l.distinct == l
  }

  /**
    * Proprietà: la toList genera una lista tale che il suo primo elemento è lo stesso restituito
    * della getMin del RBT e l'ultimo elemento è lo stesso restituito della getMax del RBT. Se la getMax o
    * la getMin sollevano un'eccezione, allora la lista e il RBT sono vuoti.
    */
  property("toList.head&&toList.reverse.head") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    val l = rbt.toList
    try {
      l.head == rbt.getMin && l.reverse.head == rbt.getMax
    } catch {
      case _: NoSuchElementException => l.isEmpty && rbt.isEmpty
    }
  }

  /**
    * Proprietà: la toList genera una lista ordinata
    */
  property("toList.sorted") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    val l = rbt.toList
    l.sorted == l
  }

  /**
    * Proprietà: la toList genera una lista con tutti e soli gli elementi del RBT
    */
  property("toList.forall.isMember&&toList.size") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    val l = rbt.toList
    l.forall(x => rbt.isMember(x)) && rbt.toList.size == rbt.size
  }

  /**
    * Proprietà: se il RBT è vuoto allora la size del RBT è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    if (rbt.isEmpty) {
      rbt.size == 0
    } else {
      rbt.size != 0
    }
  }

  /**
    * Proprietà: se la size del RBT è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genTree) { rbt: RedBlackTree[Int] =>
    if (rbt.size == 0) {
      rbt.isEmpty
    } else {
      !rbt.isEmpty
    }
  }
}
