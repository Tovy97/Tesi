import struttureDati.minHeap.PH.PairingHeap

val heap = PairingHeap(5,9,8,6,0,-1,5)
val heapVuoto = PairingHeap[Int]()

/*----size----*/
heap.size
heapVuoto.size

/*----isEmpty----*/
heap.isEmpty
heapVuoto.isEmpty

/*----insert----*/
heap.insert(-12)
heapVuoto.insert(3)

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
