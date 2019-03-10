package specificheStruttureDati

import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.queue.BQ.BatchedQueue

/**
  * Implementa le specifiche di BQ, con tutte le sue proprietà da verificare.
  */
private object BQ_Specification extends Properties("BQ") {

  /**
    * Genera BQ attraverso il metodo apply del Companion Object
    */
  private val genQueue: Gen[BatchedQueue[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield BatchedQueue(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati BQ e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: la addRight aumenta la size della coda di 1
    */
  property("addRight.size") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (bq: BatchedQueue[Int], e: Int) =>
    bq.addRight(e).size == bq.size + 1
  }

  /**
    * Proprietà: la addRight di un elemento E seguita dalla tail varia la coda, a meno che la coda non sia vuota o
    * composta dal solo elemento E ripetuto una o più volte.
    */
  property("addRight.tail") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (bq: BatchedQueue[Int], e: Int) =>
    if (bq.isEmpty) {
      bq.addRight(e).tail == bq
    } else {
      if (bq.toList.exists(x => x != e)) {
        bq.addRight(e).tail != bq
      } else {
        true
      }
    }
  }

  /**
    * Proprietà: la addRight di un elemento E seguita dalla head non ritorna E,
    * a meno che la coda non sia vuota o contenga E in testa.
    */
  property("addRight.head") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (bq: BatchedQueue[Int], e: Int) =>
    if (bq.isEmpty) {
      bq.addRight(e).head == e
    } else {
      if (bq.head == e) {
        true
      } else {
        bq.addRight(e).head != e
      }
    }
  }

  /**
    * Proprietà: la tail seguita dalla addRight dell'elemento tolto varia la coda, a meno che
    * la coda non sia composta un solo elemento ripetuto 1 o più volte. Se la head
    * solleva un'eccezione, allora la coda è vuota.
    */
  property("tail.addRight") = forAll(genQueue) { bq: BatchedQueue[Int] =>
    try {
      if (bq.toList.exists(x => x != bq.head)) {
        bq.tail.addRight(bq.head) != bq
      } else {
        true
      }
    } catch {
      case _: NoSuchElementException => bq.isEmpty
    }
  }

  /**
    * Proprietà: la tail diminuisce la size della coda di 1 se essa non è vuota, altrimenti la
    * lascia invariata.
    */
  property("tail.size") = forAll(genQueue) { bq: BatchedQueue[Int] =>
    if (bq.isEmpty) {
      bq.tail.size == bq.size
    } else {
      bq.tail.size == bq.size - 1
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa della coda
    */
  property("toList.size") = forAll(genQueue) { bq: BatchedQueue[Int] =>
    bq.toList.size == bq.size
  }

  /**
    * Proprietà: la toList genera una lista il cui primo elemento è lo stesso
    * restituito dalla head della coda. Se la head solleva un'eccezione, allora la lista
    * e la coda devono essere vuote.
    */
  property("toList.head") = forAll(genQueue) { bq: BatchedQueue[Int] =>
    val l = bq.toList
    try {
      l.head == bq.head
    } catch {
      case _: NoSuchElementException => l.isEmpty && bq.isEmpty
    }
  }

  /**
    * Proprietà: se la coda è vuota allora la size della coda è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genQueue) { bq: BatchedQueue[Int] =>
    if (bq.isEmpty) {
      bq.size == 0
    } else {
      bq.size > 0
    }
  }

  /**
    * Proprietà: se la size della coda è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genQueue) { bq: BatchedQueue[Int] =>
    if (bq.size == 0) {
      bq.isEmpty
    } else {
      !bq.isEmpty
    }
  }
}
