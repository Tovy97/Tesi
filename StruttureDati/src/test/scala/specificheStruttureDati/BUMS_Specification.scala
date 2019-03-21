package specificheStruttureDati

import org.scalacheck.Prop.forAll
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import struttureDati.sortable.BUMS.BottomUpMergeSort

private object BUMS_Specification extends Properties("BUMS") {

  /**
    * Genera BUMS attraverso il metodo apply del Companion Object
    */
  private val gen: Gen[BottomUpMergeSort[Int]] = for {
    list <- Gen.listOf[Int](Arbitrary.arbitrary[Int])
  } yield BottomUpMergeSort(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati BUMS e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni BUMS creato attraverso il metodo apply del companion object è corretto
    */
  property("isCorrect") = forAll(gen) { bums: BottomUpMergeSort[Int] =>
    bums.isCorrect
  }

  /**
    * Proprietà: la add di un elemento E in un BUMS genera un BUMS corretto
    */
  property("add.isCorrect") = forAll(gen, Arbitrary.arbitrary[Int]) { (bums: BottomUpMergeSort[Int], e: Int) =>
    bums.add(e).isCorrect
  }


  /**
    * Proprietà: la add di un elemento E in un BUMS aumenta di 1 la size del BUMS.
    */
  property("add.size") = forAll(gen, Arbitrary.arbitrary[Int]) { (bums: BottomUpMergeSort[Int], e: Int) =>
    bums.add(e).size == bums.size + 1
  }

  /**
    * Proprietà: la sort genera una lista la cui size è la stessa del BUMS
    */
  property("sort.size") = forAll(gen) { bums: BottomUpMergeSort[Int] =>
    bums.sort.size == bums.size
  }

  /**
    * Proprietà: la sort genera una lista ordinata
    */
  property("sort.sorted") = forAll(gen) { bums: BottomUpMergeSort[Int] =>
    val l = bums.sort
    l.sorted == l
  }

  /**
    * Proprietà: se il BUMS è vuoto allora la size del BUMS è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(gen) { bums: BottomUpMergeSort[Int] =>
    if (bums.isEmpty) {
      bums.size == 0
    } else {
      bums.size != 0
    }
  }

  /**
    * Proprietà: se la size del BUMS è zero allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(gen) { bums: BottomUpMergeSort[Int] =>
    if (bums.size == 0) {
      bums.isEmpty
    } else {
      !bums.isEmpty
    }
  }
}
