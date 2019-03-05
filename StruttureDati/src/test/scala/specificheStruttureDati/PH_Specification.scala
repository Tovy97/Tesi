package specificheStruttureDati


import java.util.NoSuchElementException
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.minHeap.PH.PairingHeap

/**
  * Implementa le specifiche di PH, con tutte le sue proprietà da verificare.
  */
private object PH_Specification extends Properties("PH") {

  /**
    * Genera PH attraverso il metodo apply del Companion Object
    */
  private val genHeap: Gen[PairingHeap[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield PairingHeap(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati PH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni PH creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(genHeap) { ph: PairingHeap[Int] =>
    ph.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un PH genera un PH corretto
    */
  property("insert.isCorrect") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (ph: PairingHeap[Int], e: Int) =>
    ph.insert(e).isCorrect
  }

  /**
    * Proprietà: la deleteMin dell'elemento minore in un PH genera un PH corretto
    */
  property("deleteMin.isCorrect") = forAll(genHeap) { ph: PairingHeap[Int] =>
    ph.deleteMin.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E in un PH aumenta di 1 la size del PH.
    */
  property("insert.size") = forAll(genHeap, Arbitrary.arbitrary[Int]) { (ph: PairingHeap[Int], e: Int) =>
    ph.insert(e).size == ph.size + 1
  }

  /**
    * Proprietà: la deleteMin di un elemento E in un PH diminuisce di 1 la size del PH se esso non è vuoto,
    * altrimenti la lascia invariata.
    */
  property("deleteMin.size") = forAll(genHeap) { ph: PairingHeap[Int] =>
    if (ph.isEmpty) {
      ph.deleteMin.size == ph.size
    } else {
      ph.deleteMin.size == ph.size - 1
    }
  }

  /**
    * Proprietà: il merge tra due PH genera un PH corretto
    */
  property("merge.isCorrect") = forAll(genHeap, genHeap) { (sh1: PairingHeap[Int], sh2: PairingHeap[Int]) =>
    sh1.merge(sh2).isCorrect
  }

  /**
    * Proprietà: il merge tra due PH detti P1 e P2 genera un PH la cui size è pari alla somma delle size di P1 e P2
    */
  property("merge.size") = forAll(genHeap, genHeap) { (sh1: PairingHeap[Int], sh2: PairingHeap[Int]) =>
    sh1.merge(sh2).size == sh1.size + sh2.size
  }

  /**
    * Proprietà: il merge tra due PH detti P1 e P2 genera un PH detto PM tale che la findMin di PM ritorna il minimo tra
    * le findMin di P1 e di P2. Se la findMin solleva un'eccezione allora:
    * o PM, P1 e P2 sono vuoti
    * o P1 è vuoto e la findMin di PM è uguale alla findMin di P2
    * o P2 è vuoto e la findMin di PM è uguale alla findMin di P1.
    */
  property("merge.findMin") = forAll(genHeap, genHeap) { (sh1: PairingHeap[Int], sh2: PairingHeap[Int]) =>
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
    * Proprietà: la toList genera una lista la cui size è la stessa dello PH
    */
  property("toList.size") = forAll(genHeap) { ph: PairingHeap[Int] =>
    ph.toList.size == ph.size
  }

  /**
    * Proprietà: la toList genera una lista tale che il suo primo elemento è lo stesso restituito
    * della findMin del PH. Se la findMin solleva un'eccezione, allora la lista e il PH sono vuoti.
    */
  property("toList.head") = forAll(genHeap) { ph: PairingHeap[Int] =>
    val l = ph.toList
    try {
      l.head == ph.findMin
    } catch {
      case _: NoSuchElementException => l.isEmpty && ph.isEmpty
    }
  }

  /**
    * Proprietà: se il PH è vuoto allora la size del PH è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genHeap) { ph: PairingHeap[Int] =>
    if (ph.isEmpty) {
      ph.size == 0
    } else {
      ph.size != 0
    }
  }

  /**
    * Proprietà: se la size del PH è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genHeap) { ph: PairingHeap[Int] =>
    if (ph.size == 0) {
      ph.isEmpty
    } else {
      !ph.isEmpty
    }
  }
}
