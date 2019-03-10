package specificheStruttureDati

import org.scalacheck.Properties
import org.scalacheck.Test.{Parameters, Result, checkProperties}

/**
  * Permette di verificare se tutte le proprietà di tutte le strutture dati sono rispettate.
  */
object StruttureDati_Specification extends Properties("StruttureDati") {
  include(BH_Specification)
  include(BQ_Specification)
  include(BST_Specification)
  include(BUMS_Specification)
  include(DQ_Specification)
  include(FM_Specification)
  include(LH_Specification)
  include(PH_Specification)
  include(RBT_Specification)
  include(SH_Specification)
  include(STK_Specification)

  /**
    * Esegue la check su tutte le proprietà di tutte le strutture dati e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp: Seq[(String, Result)] = checkProperties(Parameters.default, this)

  /**
    * Verifica se tutte le proprietà di tutte le strutture dati sono verificate
    *
    * @return true se tutte le proprietà di tutte le strutture dati sono verificate, altrimenti false
    */
  lazy val isAllPassed: Boolean = checkProp.forall(x => x._2.passed)

  /**
    * Ritorna una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un
    * Result che contiene le informazioni sul test solo per i test non passati.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test solo per i test non passati.
    */
  lazy val getAllNotPassedTest: Seq[(String, Result)] = checkProp.filter(x => !x._2.passed)

  /**
    * Esegue la check su tutte le proprietà struttura dati BH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_BH: Seq[(String, Result)] = BH_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati BQ e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_BQ: Seq[(String, Result)] = BQ_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati BST e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_BST: Seq[(String, Result)] = BST_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati BUMS e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_BUMS: Seq[(String, Result)] = BUMS_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati DQ e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_DQ: Seq[(String, Result)] = DQ_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati FM e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_FM: Seq[(String, Result)] = FM_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati LH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_LH: Seq[(String, Result)] = LH_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati PH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_PH: Seq[(String, Result)] = PH_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati RBT e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_RBT: Seq[(String, Result)] = RBT_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati SH e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_SH: Seq[(String, Result)] = SH_Specification.checkProp

  /**
    * Esegue la check su tutte le proprietà struttura dati STK e ritorna una sequenza
    * di coppie formate da una stringa con il nome della proprietà testata e da un Result
    * che contiene le informazioni sul test.
    *
    * @return una sequenza di coppie formate da una stringa con il nome della proprietà testata e da un Result che contiene le informazioni sul test.
    */
  lazy val checkProp_STK: Seq[(String, Result)] = STK_Specification.checkProp
}
