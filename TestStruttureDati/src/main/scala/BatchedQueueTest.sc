import struttureDati.queue.BQ.BatchedQueue

val coda = BatchedQueue(3, 7, 5, 9, 7, 33, 7, -1, 0, 37)
val codaVuota = BatchedQueue[Int]()

/*----addRight----*/
coda.addRight(97)
codaVuota.addRight(3)

/*----isEmpty----*/
coda.isEmpty
codaVuota.isEmpty

/*----size----*/
coda.size
codaVuota.size

/*----head----*/
coda.head
try{
  codaVuota.head
} catch {
  case ex : NoSuchElementException => ex.getMessage
}

/*----tail----*/
coda.tail
codaVuota.tail

/*----toList----*/
coda.toList
codaVuota.toList