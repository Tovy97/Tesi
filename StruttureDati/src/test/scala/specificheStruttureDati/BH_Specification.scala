package specificheStruttureDati

import java.util.NoSuchElementException
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.minHeap.BH.BinomialHeap

/**
  * Implementa le specifiche di BH, con tutte le sue proprietà da verificare.
  */
private object BH_Specification extends Properties("BH") {

  /**
    * Genera BH attraverso il metodo apply del Companion Object
    */
  private val genHeap: Gen[BinomialHeap[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield BinomialHeap(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati BH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni BH creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(genHeap) { bh: BinomialHeap[Int] =>
    bh.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un BH genera un BH corretto
    */
  property("insert.isCorrect") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (bh: BinomialHeap[Int], e: Int) =>
    bh.insert(e).isCorrect
  }

  /**
    * Proprietà: la deleteMin dell'elemento minore in un BH genera un BH corretto
    */
  property("deleteMin.isCorrect") = forAll(genHeap) { bh: BinomialHeap[Int] =>
    bh.deleteMin.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un BH aumenta di 1 la size del BH.
    */
  property("insert.size") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (bh: BinomialHeap[Int], e: Int) =>
    bh.insert(e).size == bh.size + 1
  }

  /**
    * Proprietà: la deleteMin di un elemento E in un BH diminuisce di 1 la size del BH se esso non è vuoto,
    * altrimenti la lascia invariata.
    */
  property("deleteMin.size") = forAll(genHeap) { bh: BinomialHeap[Int] =>
    if (bh.isEmpty) {
      bh.deleteMin.size == bh.size
    } else {
      bh.deleteMin.size == bh.size - 1
    }
  }

  /**
    * Proprietà: il merge tra due BH genera un BH corretto
    */
  property("merge.isCorrect") = forAll(genHeap, genHeap) { (bh1: BinomialHeap[Int], bh2: BinomialHeap[Int]) =>
    bh1.merge(bh2).isCorrect
  }

  /**
    * Proprietà: il merge tra due BH detti B1 e B2 genera un BH la cui size è pari alla somma delle size di B1 e B2
    */
  property("merge.size") = forAll(genHeap, genHeap) { (bh1: BinomialHeap[Int], bh2: BinomialHeap[Int]) =>
    bh1.merge(bh2).size == bh1.size + bh2.size
  }

  /**
    * Proprietà: il merge tra due BH detti B1 e B2 genera un BH detto BM tale che la findMin di BM ritorna il minimo tra
    * le findMin di B1 e di B2. Se la findMin solleva un'eccezione allora:
    * o BM, B1 e B2 sono vuoti
    * o B1 è vuoto e la findMin di BM è uguale alla findMin di B2
    * o B2 è vuoto e la findMin di BM è uguale alla findMin di B1.
    */
  property("merge.findMin") = forAll(genHeap, genHeap) { (bh1: BinomialHeap[Int], bh2: BinomialHeap[Int]) =>
    val m = bh1.merge(bh2)
    try {
      m.findMin == math.min(bh1.findMin, bh2.findMin)
    } catch {
      case _: NoSuchElementException if m.isEmpty => bh1.isEmpty && bh2.isEmpty
      case _: NoSuchElementException if bh1.isEmpty => m.findMin == bh2.findMin
      case _: NoSuchElementException => m.findMin == bh1.findMin
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa del BH
    */
  property("toList.size") = forAll(genHeap) { bh: BinomialHeap[Int] =>
    bh.toList.size == bh.size
  }

  /**
    * Proprietà: se il BH è vuoto allora la size del BH è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genHeap) { bh: BinomialHeap[Int] =>
    if (bh.isEmpty) {
      bh.size == 0
    } else {
      bh.size != 0
    }
  }

  /**
    * Proprietà: se la size del BH è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genHeap) { bh: BinomialHeap[Int] =>
    if (bh.size == 0) {
      bh.isEmpty
    } else {
      !bh.isEmpty
    }
  }
}
