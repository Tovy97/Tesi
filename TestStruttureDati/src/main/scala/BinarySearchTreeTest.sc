import struttureDati.orderedSet.BST.BinarySearchTree

val bst = BinarySearchTree(5, 9, 6, 54, 3, 8, -5, -58, 0, 6)
val sbtVuoto = BinarySearchTree[Int]()

/*----size----*/
bst.size
sbtVuoto.size

/*----isEmpty----*/
bst.isEmpty
sbtVuoto.isEmpty

/*----insert----*/
bst.insert(5) == bst //il 5 è già presente in bst
bst.insert(7)
sbtVuoto.insert(6)

/*----isMember----*/
bst.isMember(5)
bst.isMember(7)
sbtVuoto.isMember(6)

/*----delete----*/
bst.delete(5)
bst.delete(7) == bst //il 7 non è presente in bst
sbtVuoto.delete(6)

/*----getMin----*/
bst.getMin
try {
  sbtVuoto.getMin
} catch {
  case ex: NoSuchElementException => ex.getMessage
}
/*----getMax----*/
bst.getMax
try {
  sbtVuoto.getMax
} catch {
  case ex: NoSuchElementException => ex.getMessage
}

/*----isCorrect----*/
bst.isCorrect
sbtVuoto.isCorrect
bst.insert(7).isCorrect
sbtVuoto.insert(6).isCorrect
bst.delete(5).isCorrect
sbtVuoto.delete(6).isCorrect

/*----toList----*/
bst.toList
bst.toList.sorted == bst.toList
bst.toList.distinct == bst.toList
sbtVuoto.toList

/*----toString----*/
bst.toString
sbtVuoto.toString