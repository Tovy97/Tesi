import struttureDati.minHeap.BH.BinomialHeap

val heap = BinomialHeap(1, 9, 8, 3, 5, 66, 0, -5, 6, 3)
val heapVuoto = BinomialHeap[Int]()

/*----size----*/
heap.size
heapVuoto.size

/*----isEmpty----*/
heap.isEmpty
heapVuoto.isEmpty

/*----insert----*/
heap.insert(1)
heapVuoto.insert(5)

/*----merge----*/
heap.merge(heap)
heap.merge(heapVuoto) == heapVuoto.merge(heap)
heapVuoto.merge(heapVuoto)

/*----findMin----*/
heap.findMin
try {
  heapVuoto.findMin
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----deleteMin----*/
heap.deleteMin
heapVuoto.deleteMin

/*----isCorrect----*/
heap.isCorrect
heapVuoto.isCorrect
heap.insert(1).isCorrect
heapVuoto.insert(5).isCorrect
heap.merge(heap).isCorrect
heap.merge(heapVuoto).isCorrect
heapVuoto.merge(heapVuoto).isCorrect
heap.deleteMin.isCorrect
heapVuoto.deleteMin.isCorrect

/*----toList----*/
heap.toList
heapVuoto.toList

/*----toString----*/
heap.toString
heapVuoto.toString
