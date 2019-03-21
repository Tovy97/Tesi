package specificheStruttureDati

import java.util.NoSuchElementException

import org.scalacheck.Prop.forAll
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import struttureDati.splayHeap.SH.SplayHeap

/**
  * Implementa le specifiche di SH, con tutte le sue proprietà da verificare.
  */
private object SH_Specification extends Properties("SH") {

  /**
    * Esegue la check su tutte le proprietà struttura dati SH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Genera SH attraverso il metodo apply del Companion Object
    */
  private val genHeap: Gen[SplayHeap[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield SplayHeap(list: _*)

  /**
    * Proprietà: ogni SH creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(genHeap) { sh: SplayHeap[Int] =>
    sh.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in uno SH genera uno SH corretto
    */
  property("insert.isCorrect") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (sh: SplayHeap[Int], e: Int) =>
    sh.insert(e).isCorrect
  }

  /**
    * Proprietà: la deleteMin dell'elemento minore in uno SH genera uno SH corretto
    */
  property("deleteMin.isCorrect") = forAll(genHeap) { sh: SplayHeap[Int] =>
    sh.deleteMin.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in uno SH aumenta di 1 la size dello SH.
    */
  property("insert.size") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (sh: SplayHeap[Int], e: Int) =>
    sh.insert(e).size == sh.size + 1
  }

  /**
    * Proprietà: la deleteMin di un elemento E in uno SH diminuisce di 1 la size dello SH se esso non è vuoto,
    * altrimenti la lascia invariata.
    */
  property("deleteMin.size") = forAll(genHeap) { sh: SplayHeap[Int] =>
    if (sh.isEmpty) {
      sh.deleteMin.size == sh.size
    } else {
      sh.deleteMin.size == sh.size - 1
    }
  }

  /**
    * Proprietà: il merge tra due SH genera uno SH corretto
    */
  property("merge.isCorrect") = forAll(genHeap, genHeap) { (sh1: SplayHeap[Int], sh2: SplayHeap[Int]) =>
    sh1.merge(sh2).isCorrect
  }

  /**
    * Proprietà: il merge tra due SH detti S1 e S2 genera uno SH la cui size è pari alla somma delle size di S1 e S2
    */
  property("merge.size") = forAll(genHeap, genHeap) { (sh1: SplayHeap[Int], sh2: SplayHeap[Int]) =>
    sh1.merge(sh2).size == sh1.size + sh2.size
  }

  /**
    * Proprietà: il merge tra due SH detti S1 e S2 genera uno SH detto SM tale che la findMin di SM ritorna il minimo tra
    * le findMin di S1 e di S2. Se la findMin solleva un'eccezione allora:
    * o SM, S1 e S2 sono vuoti
    * o S1 è vuoto e la findMin di SM è uguale alla findMin di S2
    * o S2 è vuoto e la findMin di SM è uguale alla findMin di S1.
    */
  property("merge.findMin") = forAll(genHeap, genHeap) { (sh1: SplayHeap[Int], sh2: SplayHeap[Int]) =>
    val m = sh1.merge(sh2)
    try {
      m.findMin == math.min(sh1.findMin, sh2.findMin)
    } catch {
      case _: NoSuchElementException if m.isEmpty => sh1.isEmpty && sh2.isEmpty
      case _: NoSuchElementException if sh1.isEmpty => m.findMin == sh2.findMin
      case _: NoSuchElementException => m.findMin == sh1.findMin
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa dello SH
    */
  property("toList.size") = forAll(genHeap) { sh: SplayHeap[Int] =>
    sh.toList.size == sh.size
  }

  /**
    * Proprietà: la toList genera una lista tale che il suo primo elemento è lo stesso restituito
    * della findMin dello SH. Se la findMin solleva un'eccezione, allora la lista e lo SH sono vuoti.
    */
  property("toList.head") = forAll(genHeap) { sh: SplayHeap[Int] =>
    val l = sh.toList
    try {
      l.head == sh.findMin
    } catch {
      case _: NoSuchElementException => l.isEmpty && sh.isEmpty
    }
  }

  /**
    * Proprietà: la toList genera una lista ordinata
    */
  property("toList.sorted") = forAll(genHeap) { sh: SplayHeap[Int] =>
    val l = sh.toList
    l.sorted == l
  }

  /**
    * Proprietà: se lo SH è vuoto allora la size dello SH è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genHeap) { sh: SplayHeap[Int] =>
    if (sh.isEmpty) {
      sh.size == 0
    } else {
      sh.size != 0
    }
  }

  /**
    * Proprietà: se la size dello SH è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genHeap) { sh: SplayHeap[Int] =>
    if (sh.size == 0) {
      sh.isEmpty
    } else {
      !sh.isEmpty
    }
  }
}
