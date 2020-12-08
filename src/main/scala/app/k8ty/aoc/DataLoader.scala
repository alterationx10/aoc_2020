package app.k8ty.aoc

trait DataLoader {

  def loadData(rsrc: String): Iterator[String] = {
    scala.io.Source.fromResource(rsrc)
      .getLines()
  }

}
