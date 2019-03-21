import specificheStruttureDati.StruttureDati_Specification
import struttureDati.queue.BQ.BatchedQueue
import struttureDati.queue.DQ.Deque

StruttureDati_Specification.isAllPassed
StruttureDati_Specification.getAllNotPassedTest

val coda = Deque(2 :: Nil, 4 :: 3 :: Nil)
val coda2 = Deque(2 :: 3 :: Nil, 4 :: Nil)

coda == coda2

coda.toString
coda2.toString

val codaB1 = BatchedQueue().addRight(1).addRight(2)
val codaB2 = codaB1.addRight(1).addRight(2).tail.tail

codaB1 == codaB2

codaB1.toString
codaB2.toString