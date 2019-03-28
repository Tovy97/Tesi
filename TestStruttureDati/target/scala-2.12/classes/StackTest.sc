import struttureDati.stack.STK.Stack

val pila = Stack(5, 0, -5, 9, 8, 6, 7, 5)
val pilaVuota = Stack[Int]()

/*----size----*/
pila.size
pilaVuota.size

/*----isEmpty----*/
pila.isEmpty
pilaVuota.isEmpty

/*----pop----*/
pila.pop
pilaVuota.pop

/*----push----*/
pila.push(7)
pilaVuota.push(3)

/*----top----*/
pila.top
try {
  pilaVuota.top
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----toList----*/
pila.toList
pilaVuota.toList

/*----toString----*/
pila.toString
pilaVuota.toString
