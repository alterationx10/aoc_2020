package app.k8ty.aoc


object Day6 extends App with DataLoader {

  val separator = "---"
  def addSeparator: String => String = str => if (str.isEmpty) separator else str

  lazy val testDataPart1 = loadData("day_6.test")
    .map(addSeparator)
    .mkString("")
    .split(separator)

  lazy val liveDataPart1 = loadData("day_6.data")
    .map(addSeparator)
    .mkString("")
    .split(separator)

  val part1Test = testDataPart1.map(_.toSet.size).sum
  val part1Live = liveDataPart1.map(_.toSet.size).sum

  println(s"There were ${part1Test} 'yes's for Part 1 Test")
  println(s"There were ${part1Live} 'yes's for Part 1 Live")

  def groupOfGroups: String => List[List[Char]] = str => str.trim.split(" ").toList.map(_.toList)
  def foldCommon: List[List[Char]] => List[Char] = list => list.fold(list.head)(_.intersect(_))

  lazy val testDataPart2 = loadData("day_6.test")
    .map(addSeparator)
    .mkString(" ")
    .split(separator)
    .map(groupOfGroups)
    .map(foldCommon)
    .map(_.length)
    .sum


  lazy val liveDataPart2 = loadData("day_6.data")
    .map(addSeparator)
    .mkString(" ")
    .split(separator)
    .map(groupOfGroups)
    .map(foldCommon)
    .map(_.length)
    .sum


  println(s"There were ${testDataPart2} common 'yes's for Part 2 Test")
  println(s"There were ${liveDataPart2} common 'yes's for Part 2 Live")


}
