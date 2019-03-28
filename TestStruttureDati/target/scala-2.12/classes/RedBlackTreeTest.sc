import struttureDati.orderedSet.RBT.RedBlackTree

val rbt = RedBlackTree(5, 9, 6, 54, 3, 8, -5, -58, 0, 6)
val rbtVuoto = RedBlackTree[Int]()

/*----size----*/
rbt.size
rbtVuoto.size

/*----isEmpty----*/
rbt.isEmpty
rbtVuoto.isEmpty

/*----insert----*/
rbt.insert(5) == rbt //il 5 è già presente in rbt
rbt.insert(7)
rbtVuoto.insert(6)

/*----isMember----*/
rbt.isMember(5)
rbt.isMember(7)
rbtVuoto.isMember(6)

/*----delete----*/
rbt.delete(5)
rbt.delete(7) == rbt //il 7 non è presente in rbt
rbtVuoto.delete(6)

/*----getMin----*/
rbt.getMin
try {
  rbtVuoto.getMin
} catch {
  case ex: NoSuchElementException => ex.getMessage
}
/*----getMax----*/
rbt.getMax
try {
  rbtVuoto.getMax
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----isCorrect----*/
rbt.isCorrect
rbtVuoto.isCorrect
rbt.insert(7).isCorrect
rbtVuoto.insert(6).isCorrect
rbt.delete(5).isCorrect
rbtVuoto.delete(6).isCorrect

/*----toList----*/
rbt.toList
rbt.toList.sorted == rbt.toList
rbt.toList.distinct == rbt.toList
rbtVuoto.toList

/*----toString----*/
rbt.toString
rbtVuoto.toString