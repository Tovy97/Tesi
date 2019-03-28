import struttureDati.queue.DQ.Deque


val codaDoppia = Deque(9, 0, -5, 8, 6, -55, 5)
val codaDoppiaVuoto = Deque[Int]()

/*----size----*/
codaDoppia.size
codaDoppiaVuoto.size

/*----isEmpty----*/
codaDoppia.isEmpty
codaDoppiaVuoto.isEmpty

/*----addRight----*/
codaDoppia.addRight(5)
codaDoppiaVuoto.addRight(3)

/*----head----*/
codaDoppia.head
try {
  codaDoppiaVuoto.head
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----tail----*/
codaDoppia.tail
codaDoppiaVuoto.tail

/*----last----*/
codaDoppia.last
try {
  codaDoppiaVuoto.last
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----init----*/
codaDoppia.init
codaDoppiaVuoto.init

/*----addLeft----*/
codaDoppia.addLeft(5)
codaDoppiaVuoto.addLeft(3)

/*----toList----*/
codaDoppia.toList
codaDoppiaVuoto.toList

/*----toString----*/
codaDoppia.toString
codaDoppiaVuoto.toString
