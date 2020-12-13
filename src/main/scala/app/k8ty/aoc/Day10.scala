package app.k8ty.aoc


object Day10 extends App with DataLoader {

  lazy val testData = loadData("day_10.test").map(_.toInt).toSeq.sorted
  lazy val part1TestData = 0 +: testData :+ testData.max + 3

  lazy val liveData = loadData("day_10.data").map(_.toInt).toSeq.sorted
  lazy val part1LiveData = 0 +: liveData :+ liveData.max + 3

  def offBy(target: Int, data: Seq[Int], count: Int): Int = data match {
    case Nil => count
    case h :: Nil => count
    case h :: t => (t.head - h == target) match {
      case true => offBy(target, t, count + 1)
      case false => offBy(target, t, count)
    }
  }

  val t11 = offBy(1, part1TestData, 0)
  val t12 = offBy(2, part1TestData, 0)
  val t13 = offBy(3, part1TestData, 0)
  println(s"Part 1 test off by 1: ${t11}")
  println(s"Part 1 test off by 2: ${t12}")
  println(s"Part 1 test off by 3: ${t13}")
  val p11 = offBy(1, part1LiveData, 0)
  val p13 = offBy(3, part1LiveData, 0)
  println(s"Part 1 live off by 1: $p11")
  println(s"Part 1 live off by 3: $p13")
  println(s"Part 1 answer: ${p11 * p13}")


  // Wasn't able to get this on my own 😞.
  // Based off of this wonderful work:
  // https://github.com/kbielefe/advent-of-code/blob/master/src/main/scala/2020/10.scala

  def view(input: Seq[Int]): Long = {
    val init = Map(input.head -> 1L)
    val counts: Map[Int, Long] = input.tail.foldLeft(init) { case (a, b) =>
      val nextThree = b + 1 to b + 3
      val seen: Long = nextThree.map(a.getOrElse(_, 0L)).sum
      a + (b -> seen)
    }
    counts.values.max
  }

  println(view(part1TestData.reverse))
  println(view(part1LiveData.reverse))
}
