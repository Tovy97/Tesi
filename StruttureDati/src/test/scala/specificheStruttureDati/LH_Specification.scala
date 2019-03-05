package specificheStruttureDati

import java.util.NoSuchElementException
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.minHeap.LH.LeftistHeap

/**
  * Implementa le specifiche di LH, con tutte le sue proprietà da verificare.
  */
private object LH_Specification extends Properties("LH") {

  /**
    * Genera LH attraverso il metodo apply del Companion Object
    */
  private val genHeap: Gen[LeftistHeap[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield LeftistHeap(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati LH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni LH creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    lh.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un LH genera un LH corretto
    */
  property("insert.isCorrect") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (lh: LeftistHeap[Int], e: Int) =>
    lh.insert(e).isCorrect
  }

  /**
    * Proprietà: la deleteMin dell'elemento minore in un LH genera un LH corretto
    */
  property("deleteMin.isCorrect") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    lh.deleteMin.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un LH aumenta di 1 la size del LH.
    */
  property("insert.size") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (lh: LeftistHeap[Int], e: Int) =>
    lh.insert(e).size == lh.size + 1
  }

  /**
    * Proprietà: la deleteMin di un elemento E in un LH diminuisce di 1 la size del LH se esso non è vuoto,
    * altrimenti la lascia invariata.
    */
  property("deleteMin.size") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    if (lh.isEmpty) {
      lh.deleteMin.size == lh.size
    } else {
      lh.deleteMin.size == lh.size - 1
    }
  }

  /**
    * Proprietà: il merge tra due LH genera un LH corretto
    */
  property("merge.isCorrect") = forAll(genHeap, genHeap) { (sh1: LeftistHeap[Int], sh2: LeftistHeap[Int]) =>
    sh1.merge(sh2).isCorrect
  }

  /**
    * Proprietà: il merge tra due LH detti L1 e L2 genera un LH la cui size è pari alla somma delle size di L1 e L2
    */
  property("merge.size") = forAll(genHeap, genHeap) { (sh1: LeftistHeap[Int], sh2: LeftistHeap[Int]) =>
    sh1.merge(sh2).size == sh1.size + sh2.size
  }

  /**
    * Proprietà: il merge tra due LH detti L1 e L2 genera un LH detto LM tale che la findMin di LM ritorna il minimo tra
    * le findMin di L1 e di L2. Se la findMin solleva un'eccezione allora:
    * o LM, L1 e L2 sono vuoti
    * o L1 è vuoto e la findMin di LM è uguale alla findMin di L2
    * o L2 è vuoto e la findMin di LM è uguale alla findMin di L1.
    */
  property("merge.findMin") = forAll(genHeap, genHeap) { (sh1: LeftistHeap[Int], sh2: LeftistHeap[Int]) =>
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
    * Proprietà: la toList genera una lista la cui size è la stessa del LH
    */
  property("toList.size") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    lh.toList.size == lh.size
  }

  /**
    * Proprietà: la toList genera una lista tale che il suo primo elemento è lo stesso restituito
    * della findMin del LH. Se la findMin solleva un'eccezione, allora la lista è vuota.
    */
  property("toList.head") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    val l = lh.toList
    try {
      l.head == lh.findMin
    } catch {
      case _: NoSuchElementException => l.isEmpty && lh.isEmpty
    }
  }

  /**
    * Proprietà: se il LH è vuoto allora la size del LH è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    if (lh.isEmpty) {
      lh.size == 0
    } else {
      lh.size != 0
    }
  }

  /**
    * Proprietà: se la size del LH è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genHeap) { lh: LeftistHeap[Int] =>
    if (lh.size == 0) {
      lh.isEmpty
    } else {
      !lh.isEmpty
    }
  }
}
