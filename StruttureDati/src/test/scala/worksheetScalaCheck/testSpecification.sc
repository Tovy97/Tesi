import specificheStruttureDati.StruttureDati_Specification
import struttureDati.queue.DQ.Deque

StruttureDati_Specification.isAllPassed
StruttureDati_Specification.getAllNotPassedTest

var coda = Deque(2 :: Nil, 3 :: Nil)
var coda2 = Deque(2 :: 3 :: Nil, Nil)
var coda3 = Deque(Nil, 3 :: 2 :: Nil)

coda == coda2
coda == coda3
coda2 == coda3

coda.toString
coda2.toString
coda3.toString