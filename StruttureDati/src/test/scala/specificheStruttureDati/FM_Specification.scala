package specificheStruttureDati

import org.scalacheck.Prop.forAll
import org.scalacheck.Test.{Parameters, Result, checkProperties}
import org.scalacheck.{Arbitrary, Gen, Properties}
import struttureDati.map.FM.FiniteMap

/**
  * Implementa le specifiche di FM, con tutte le sue proprietà da verificare.
  */
private object FM_Specification extends Properties("FM") {

  /**
    * Genera coppie di interi
    */
  private val genTuple: Gen[(Int, Int)] = for {
    x <- Arbitrary.arbitrary[Int]
    y <- Arbitrary.arbitrary[Int]
  } yield (x, y)

  /**
    * Genera FM attraverso il metodo apply del Companion Object
    */
  private val genMap: Gen[FiniteMap[Int, Int]] = for {
    list <- Gen.listOf[(Int, Int)](genTuple)
  } yield FiniteMap(list: _*)

  /**
    * Esegue la check su tutte le proprietà struttura dati FM e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Proprietà: ogni FM creata attraverso il metodo apply del companion object è corretta
    */
  property("isCorrect") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    fm.isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E con chiave I in una FM genera una FM corretta
    */
  property("insert.isCorrect_1") = forAll(genMap, genTuple) { (fm: FiniteMap[Int, Int], e: (Int, Int)) =>
    fm.insert(e).isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento passato come coppia o come due parametri distinti
    * genera la stessa FM
    */
  property("insert.isCorrect_2") = forAll(genMap, genTuple) { (fm: FiniteMap[Int, Int], e: (Int, Int)) =>
    fm.insert(e._1, e._2) == fm.insert(e)
  }

  /**
    * Proprietà: la delete di un elemento con chiave I in una FM genera una FM corretta
    */
  property("delete.isCorrect") = forAll(genMap, Arbitrary.arbitrary[Int]) { (fm: FiniteMap[Int, Int], e: Int) =>
    fm.delete(e).isCorrect
  }

  /**
    * Proprietà: l'insert di un elemento E con chiave I in una FM aumenta di 1 la size della FM se la FM non
    * contiene già un elemento con quella chiave (ovvero se la isKeyUsed ritorna false), altrimenti la lascia
    * invariata.
    */
  property("insert.size") = forAll(genMap, genTuple) { (fm: FiniteMap[Int, Int], e: (Int, Int)) =>
    if (fm.isKeyUsed(e._1)) {
      fm.insert(e).size == fm.size
    } else {
      fm.insert(e).size == fm.size + 1
    }
  }

  /**
    * Proprietà: la delete di un elemento E con chiave I in una FM diminuisce di 1 la size della FM se la FM
    * contiene un elemento con quella chiave (ovvero se la isKeyUsed ritorna true), altrimenti la lascia
    * invariata.
    */
  property("delete.size") = forAll(genMap, Arbitrary.arbitrary[Int]) { (fm: FiniteMap[Int, Int], e: Int) =>
    if (fm.isKeyUsed(e)) {
      fm.delete(e).size == fm.size - 1
    } else {
      fm.delete(e).size == fm.size
    }
  }

  /**
    * Proprietà: la toList genera una lista la cui size è la stessa della FM
    */
  property("toList.size") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    fm.toList.size == fm.size
  }

  /**
    * Proprietà: la toList genera una lista senza elementi ripetuti
    */
  property("toList.distinct") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    val l = fm.toList
    l.distinct == l
  }

  /**
    * Proprietà: la toList genera una lista tale che il suo primo elemento è la stessa coppia restituita
    * della getMin della FM e l'ultimo elemento è la stessa coppia restituita della getMax della FM. Se la getMax o
    * la getMin sollevano un'eccezione, allora la lista e la FM sono vuote.
    */
  property("toList.head&&toList.reverse.head") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    val l = fm.toList
    try {
      l.head == fm.getMin && l.reverse.head == fm.getMax
    } catch {
      case _: NoSuchElementException => l.isEmpty && fm.isEmpty
    }
  }

  /**
    * Proprietà: la toList genera una lista ordinata secondo la chiave
    */
  property("toList.sorted") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    val l = fm.toList
    l.sorted == l
  }

  /**
    * Proprietà: la toList genera una lista con tutti e soli gli elementi della FM
    */
  property("toList.forall.isKeyUsed&&toList.size") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    val l = fm.toList
    l.forall(x => fm.isKeyUsed(x._1) && fm.getElement(x._1) == x._2) && fm.toList.size == fm.size
  }

  /**
    * Proprietà: se la FM è vuota allora la size della FM è 0,
    * altrimenti è strettamente maggiore di 0
    */
  property("isEmpty&&size") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    if (fm.isEmpty) {
      fm.size == 0
    } else {
      fm.size != 0
    }
  }

  /**
    * Proprietà: se la size della FM è 0 allora isEmpty ritorna true,
    * altrimenti ritorna false
    */
  property("size&&isEmpty") = forAll(genMap) { fm: FiniteMap[Int, Int] =>
    if (fm.size == 0) {
      fm.isEmpty
    } else {
      !fm.isEmpty
    }
  }
}
