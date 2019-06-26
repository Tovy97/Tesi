import struttureDati.sortable.BUMS.BottomUpMergeSort

val bums = BottomUpMergeSort(5, 0, -5, 9, 7, 5)
val bumsVuoto = BottomUpMergeSort[Int]()

/*----size----*/
bums.size
bumsVuoto.size

/*----isEmpty----*/
bums.isEmpty
bumsVuoto.isEmpty

/*----add----*/
bums.add(3)
bumsVuoto.add(7)

/*----sort----*/
bums.sort
bumsVuoto.sort

/*----isCorrect----*/
bums.isCorrect
bumsVuoto.isCorrect

/*----toString----*/
bums.toString
bumsVuoto.toString
