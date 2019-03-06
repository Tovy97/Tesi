import struttureDati.map.FM.FiniteMap

val mappa = FiniteMap((5, 8), (9, 0), (-5, 0), (0, 9), (-7, -6))
val mappaVuota = FiniteMap[Int, Int]()

/*----size----*/
mappa.size
mappaVuota.size

/*----isEmpty----*/
mappa.isEmpty
mappaVuota.isEmpty

/*----insert----*/
mappa.insert(1, 1)
mappa.insert(1, 1) == mappa.insert((1, 1))
mappa.insert(5, 8) == mappa
mappa.insert(5, 0) == mappa
mappaVuota.insert(3, 7)

/*----isKeyUsed----*/
mappa.isKeyUsed(5)
mappa.isKeyUsed(2)
mappaVuota.isKeyUsed(1)

/*----delete----*/
mappa.delete(5)
mappa.delete(2) == mappa
mappaVuota.delete(1)

/*----getElement----*/
mappa.getElement(5)
try {
  mappa.getElement(2)
} catch {
  case ex: NoSuchElementException => ex.getMessage
}
try {
  mappaVuota.getElement(1)
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----getMax----*/
mappa.getMax
try {
  mappaVuota.getMax
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----getMin----*/
mappa.getMin
try {
  mappaVuota.getMin
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----isCorrect----*/
mappa.isCorrect
mappaVuota.isCorrect
mappa.insert(1, 1).isCorrect
mappa.insert(5, 0).isCorrect
mappaVuota.insert(3, 7).isCorrect
mappa.delete(5).isCorrect
mappaVuota.delete(1).isCorrect

/*----toList----*/
mappa.toList
mappa.toList.sorted == mappa.toList
mappa.toList.distinct == mappa.toList
mappaVuota.toList

/*----toString----*/
mappa.toString
mappaVuota.toString
