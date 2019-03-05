package specificheStruttureDati

import org.scalacheck.Test.{Parameters, checkProperties, Result}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import struttureDati.queue.DQ.Deque

/**
  * Implementa le specifiche di DQ, con tutte le sue proprietà da verificare.
  */
private object DQ_Specification extends Properties("DQ") {

  /**
    * Genera DQ attraverso il metodo apply del Companion Object
    */
  private lazy val genQueue: Gen[Deque[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield Deque(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati DQ e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: la addRight aumenta la size della coda di 1
    */
  property("addRight.size") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    dq.addRight(e).size == dq.size + 1
  }

  /**
    * Proprietà: la addLeft aumenta la size della coda di 1
    */
  property("addLeft.size") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    dq.addLeft(e).size == dq.size + 1
  }

  /**
    * Proprietà: la addLeft di un elemento E seguita dalla init varia la coda, a meno che la coda non sia
    * vuota o composta dal solo elemento E ripetuto una o più volte.
    */
  property("addLeft.init") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    if (dq.isEmpty) {
      dq.addLeft(e).init.toList == dq.toList
    } else {
      if (dq.toList.exists(x => x != e)) {
        dq.addLeft(e).init.toList != dq.toList
      } else {
        true
      }
    }
  }

  /**
    * Proprietà: la addLeft di un elemento E seguita dalla tail non varia la coda.
    */
  property("addLeft.tail") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    dq.addLeft(e).tail.toList == dq.toList
  }

  /**
    * Proprietà: la addRight di un elemento E seguita dalla init non varia la coda.
    */
  property("addRight.init") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    dq.addRight(e).init.toList == dq.toList
  }

  /**
    * Proprietà: la addRight di un elemento E seguita dalla tail varia la coda, a meno che la coda non sia
    * vuota o composta dal solo elemento E ripetuto una o più volte.
    */
  property("addRight.tail") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    if (dq.isEmpty) {
      dq.addRight(e).tail.toList == dq.toList
    } else {
      if (dq.toList.exists(x => x != e)) {
        dq.addRight(e).tail.toList != dq.toList
      } else {
        true
      }
    }
  }

  /**
    * Proprietà: la addRight di un elemento E seguita dalla head non ritorna E,
    * a meno che la coda non sia vuota o contenga in testa E.
    */
  property("addRight.head") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    if (dq.isEmpty) {
      dq.addRight(e).head == e
    } else {
      if (dq.head == e) {
        true
      } else {
        dq.addRight(e).head != e
      }
    }
  }

  /**
    * Proprietà: la addRight di un elemento E seguita dalla last ritorna E.
    */
  property("addRight.last") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    dq.addRight(e).last == e
  }

  /**
    * Proprietà: la addLeft di un elemento E seguita dalla head ritorna E.
    */
  property("addLeft.head") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    dq.addLeft(e).head == e
  }

  /**
    * Proprietà: la addLeft di un elemento E seguita dalla last non ritorna E,
    * a meno che la coda non sia vuota o contenga in coda E.
    */
  property("addLeft.last") = forAll(genQueue, Arbitrary.arbitrary[Int]) { (dq: Deque[Int], e: Int) =>
    if (dq.isEmpty) {
      dq.addLeft(e).last == e
    } else {
      if (dq.last == e) {
        true
      } else {
        dq.addLeft(e).last != e
      }
    }
  }

  /**
    * Proprietà: la tail seguita dalla addRight dell'elemento tolto varia la coda, a meno che
    * la coda non sia composta un solo elemento ripetuto 1 o più volte. Se la head (usata per ottenere
    * l’elemento tolto dalla tail) solleva un'eccezione, allora la coda è vuota.
    */
  property("tail.addRight") = forAll(genQueue) { dq: Deque[Int] =>
    try {
      if (dq.toList.exists(x => x != dq.head)) {
        dq.tail.addRight(dq.head).toList != dq.toList
      } else {
        true
      }
    } catch {
      case _: NoSuchElementException => dq.isEmpty
    }
  }

  /**
    * Proprietà: la init seguita dalla addLeft dell'elemento tolto varia la coda, a meno che
    * la coda non sia composta un solo elemento ripetuto 1 o più volte. Se la last (usata per ottenere
    * l’elemento tolto dalla init) solleva un'eccezione, allora la coda è vuota.
    */
  property("init.addLeft") = forAll(genQueue) { dq: Deque[Int] =>
    try {
      if (dq.toList.exists(x => x != dq.last)) {
        dq.init.addLeft(dq.last).toList != dq.toList
      } else {
        true
      }
    } catch {
      case _: NoSuchElementException => dq.isEmpty
    }
  }

  /**
    * Proprietà: la init seguita dalla addRight dell'elemento tolto non varia la coda. Se la last (usata
    * per ottenere l’elemento tolto dalla init) solleva un'eccezione, allora la coda è vuota.
    */
  property("init.addRight") = forAll(genQueue) { dq: Deque[Int] =>
    try {
      dq.init.addRight(dq.last).toList == dq.toList
    } catch {
      case _: NoSuchElementException => dq.isEmpty
    }
  }

  /**
    * Proprietà: la tail seguita dalla addLeft dell'elemento tolto non varia la coda. Se la head (usata
    * per ottenere l’elemento tolto dalla tail) solleva un'eccezione, allora la coda è vuota.
    */
  property("tail.addLeft") = forAll(genQueue) { dq: Deque[Int] =>
    try {
      dq.tail.addLeft(dq.head).toList == dq.toList
    } catch {
      case _: NoSuchElementException => dq.isEmpty
    }
  }

  /**
    * Proprietà: la tail diminuisce la size della coda di 1 se essa non è vuota,
    * altrimenti la lascia invariata.
    */
  property("tail.size") = forAll(genQueue) { dq: Deque[Int] =>
    if (dq.isEmpty) {
      dq.tail.size == dq.size
    } else {
      dq.tail.size == dq.size - 1
    }
  }

  /**
    * Proprietà: la init diminuisce la size della coda di 1 se essa non è vuota,
    * altrimenti la lascia invariata.
    */
  property("init.size") = forAll(genQueue) { dq: Deque[Int] =>
    if (dq.isEmpty) {
      dq.init.size == dq.size
    } else {
      dq.init.size == dq.size - 1
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa della coda
    */
  property("toList.size") = forAll(genQueue) { dq: Deque[Int] =>
    dq.toList.size == dq.size
  }

  /**
    * Proprietà: la toList genera una lista il cui primo elemento è lo stesso
    * restituito dalla head della coda e l'ultimo è lo stesso restituito dalla last della coda. Se la
    * head o la last sollevano un'eccezione, allora la lista e la coda devono essere vuote.
    */
  property("toList.head&&toList.reverse.head") = forAll(genQueue) { dq: Deque[Int] =>
    val l = dq.toList
    try {
      l.head == dq.head && l.reverse.head == dq.last
    } catch {
      case _: NoSuchElementException => l.isEmpty && dq.isEmpty
    }
  }

  /**
    * Proprietà: se la coda è vuota allora la size della coda è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genQueue) { dq: Deque[Int] =>
    if (dq.isEmpty) {
      dq.size == 0
    } else {
      dq.size > 0
    }
  }

  /**
    * Proprietà: se la size della coda è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genQueue) { dq: Deque[Int] =>
    if (dq.size == 0) {
      dq.isEmpty
    } else {
      !dq.isEmpty
    }
  }
}
