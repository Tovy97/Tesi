package struttureDati.orderedSet.RBT

/**
  * Rappresenta il colore dei nodi di un albero reb-black.
  */
sealed trait Color

/**
  * Rappresenta il colore rosso dei nodi di un albero reb-black.
  */
private case object Red extends Color

/**
  * Rappresenta il colore nero dei nodi di un albero reb-black.
  */
private case object Black extends Color