package specificheStruttureDati

import org.scalacheck.Prop.forAll
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import struttureDati.stack.STK.Stack

/**
  * Implementa le specifiche di STK, con tutte le sue proprietà da verificare.
  */
private object STK_Specification extends Properties("STK") {

  /**
    * Genera STK attraverso il metodo apply del Companion Object
    */
  private val genSTK: Gen[Stack[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield Stack(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati STK e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: la push aumenta la size dello stack di 1
    */
  property("push.size") = forAll(genSTK, Arbitrary.arbitrary[Int]) { (stk: Stack[Int], e: Int) =>
    stk.push(e).size == stk.size + 1
  }

  /**
    * Proprietà: la push seguita dalla pop non varia lo stack
    */
  property("push.pop") = forAll(genSTK, Arbitrary.arbitrary[Int]) { (stk: Stack[Int], e: Int) =>
    stk.push(e).pop == stk
  }

  /**
    * Proprietà: la push di un elemento E seguita dalla pop ritorna E
    */
  property("push.top") = forAll(genSTK, Arbitrary.arbitrary[Int]) { (stk: Stack[Int], e: Int) =>
    stk.push(e).top == e
  }

  /**
    * Proprietà: la pop seguita dalla push dell'elemento tolto dalla pop non varia lo stack.
    * Se la top (usata per ottenere l’elemento tolto dalla pop) solleva un'eccezione, allora lo stack è vuoto.
    */
  property("pop.push") = forAll(genSTK) { stk: Stack[Int] =>
    try {
      stk.pop.push(stk.top) == stk
    } catch {
      case _: NoSuchElementException => stk.isEmpty
    }
  }

  /**
    * Proprietà: la pop diminuisce la size dello stack di 1. Se la pop solleva
    * un'eccezione, allora lo stack è vuoto.
    */
  property("pop.size") = forAll(genSTK) { stk: Stack[Int] =>
    if (stk.isEmpty) {
      stk.pop.size == stk.size
    } else {
      stk.pop.size == stk.size - 1
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa dello stack
    */
  property("toList.size") = forAll(genSTK) { stk: Stack[Int] =>
    stk.toList.size == stk.size
  }

  /**
    * Proprietà: la toList genera una lista il cui primo elemento è lo stesso
    * restituito dalla top. Se la top solleva un'eccezione, allora la lista e lo stack
    * sono vuota.
    */
  property("toList.head") = forAll(genSTK) { stk: Stack[Int] =>
    val l = stk.toList
    try {
      l.head == stk.top
    } catch {
      case _: NoSuchElementException => l.isEmpty && stk.isEmpty
    }
  }

  /**
    * Proprietà: se lo stack è vuoto allora la size dello stack è 0,
    * altrimenti è strettamente strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genSTK) { stk: Stack[Int] =>
    if (stk.isEmpty) {
      stk.size == 0
    } else {
      stk.size > 0
    }
  }

  /**
    * Proprietà: se la size dello stack è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genSTK) { stk: Stack[Int] =>
    if (stk.size == 0) {
      stk.isEmpty
    } else {
      !stk.isEmpty
    }
  }
}
