package specificheStruttureDati

import org.scalacheck.Test.{Parameters, checkProperties, Result}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.orderedSet.BST.BinarySearchTree

/**
  * Implementa le specifiche di BST, con tutte le sue proprietà da verificare.
  */
private object BST_Specification extends Properties("BST") {

  /**
    * Genera BST attraverso il metodo apply del Companion Object
    */
  private val genTree: Gen[BinarySearchTree[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield BinarySearchTree(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati BST e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni BST creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    bst.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un BST genera un BST corretto
    */
  property("insert.isCorrect") = forAll(genTree, Arbitrary.arbitrary[Int]) { (bst: BinarySearchTree[Int], e: Int) =>
    bst.insert(e).isCorrect
  }

  /**
    * Proprietà: la delete di un elemento E in un BST genera un BST corretto
    */
  property("delete.isCorrect") = forAll(genTree, Arbitrary.arbitrary[Int]) { (bst: BinarySearchTree[Int], e: Int) =>
    bst.delete(e).isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un BST aumenta di 1 la size del BST se il BST non
    * contiene già quell'elemento (ovvero se la isMember ritorna false), altrimenti la lascia
    * invariata.
    */
  property("insert.size") = forAll(genTree, Arbitrary.arbitrary[Int]) { (bst: BinarySearchTree[Int], e: Int) =>
    if (bst.isMember(e)) {
      bst.insert(e).size == bst.size
    } else {
      bst.insert(e).size == bst.size + 1
    }
  }

  /**
    * Proprietà: la delete di un elemento E in un BST diminuisce di 1 la size del BST se il
    * BST contiene quell'elemento (ovvero se la isMember ritorna true), altrimenti la lascia
    * invariata.
    */
  property("delete.size") = forAll(genTree, Arbitrary.arbitrary[Int]) { (bst: BinarySearchTree[Int], e: Int) =>
    if (bst.isMember(e)) {
      bst.delete(e).size == bst.size - 1
    } else {
      bst.delete(e).size == bst.size
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa del BST
    */
  property("toList.size") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    bst.toList.size == bst.size
  }

  /**
    * Proprietà: la toList genera una lista senza elementi ripetuti
    */
  property("toList.distinct") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    val l = bst.toList
    l.distinct == l
  }

  /**
    * Proprietà: la toList genera una lista tale che il suo primo elemento è lo stesso restituito
    * della getMin del BST e l'ultimo elemento è lo stesso restituito della getMax del BST. Se la getMax o
    * la getMin sollevano un'eccezione, allora la lista e il BST sono vuoti.
    */
  property("toList.head&&toList.reverse.head") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    val l = bst.toList
    try {
      l.head == bst.getMin && l.reverse.head == bst.getMax
    } catch {
      case _: NoSuchElementException => l.isEmpty && bst.isEmpty
    }
  }

  /**
    * Proprietà: la toList genera una lista ordinata
    */
  property("toList.sorted") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    val l = bst.toList
    l.sorted == l
  }

  /**
    * Proprietà: la toList genera una lista con tutti e soli gli elementi del BST
    */
  property("toList.forall.isMember&&toList.size") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    val l = bst.toList
    l.forall(x => bst.isMember(x)) && bst.toList.size == bst.size
  }

  /**
    * Proprietà: se il BST è vuoto allora la size del BST è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    if (bst.isEmpty) {
      bst.size == 0
    } else {
      bst.size != 0
    }
  }

  /**
    * Proprietà: se la size del BST è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genTree) { bst: BinarySearchTree[Int] =>
    if (bst.size == 0) {
      bst.isEmpty
    } else {
      !bst.isEmpty
    }
  }
}
