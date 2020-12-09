package app.k8ty.aoc

object Day5 extends App with DataLoader {

  def halfLength[T](l : String, s: Seq[T]): Seq[T] = l match {
    case "F" => s.take(s.length/2)
    case "L" => s.take(s.length/2)
    case "B" => s.takeRight(s.length/2)
    case "R" => s.takeRight(s.length/2)
    case _ => throw new Exception(":sad_trombone:")
  }

  def parseCode(code: String, seats: Seq[Int]): Int = code.length match {
    case 0 => seats.head
    case _ => parseCode(code.tail, halfLength(code.head.toString, seats))
  }

  lazy val data = loadData("day_5.data").toSeq

  lazy val part1 = data.map { str =>
    parseCode(str.take(7), 0 to 127) * 8 + parseCode(str.takeRight(3), 0 to 7)
  }.sorted

  println(s"The highest seat id is ${part1.max}")

  lazy val missing = (part1.min to part1.max).diff(part1)
  lazy val hopefullyJustOne = missing.filter(d => part1.contains(d + 1) && part1.contains(d - 1))

  println(s"Your seat id is in the set of ${hopefullyJustOne}")

}
