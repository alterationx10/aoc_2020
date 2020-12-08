package app.k8ty.aoc

import scala.annotation.tailrec

object Day3 extends App with DataLoader {

  type LocalGeology = Array[LazyList[Char]]

  lazy val day3Test: LocalGeology = loadData("day_3.test")
    .map(str => LazyList.continually(str.to(LazyList)).flatten)
    .toArray

  lazy val day3Data: LocalGeology= loadData("day_3.data")
    .map(str => LazyList.continually(str.to(LazyList)).flatten)
    .toArray

  @tailrec
  def loop(data: LocalGeology, stepsRight: Int, stepsDown: Int, stepsRightSoFar: Int, stepsDownSoFar: Int, treesSoFar: Int): Int = stepsDown match {
    case done if stepsDownSoFar >= data.length - 1 => treesSoFar
    case _ => {
      val row = data(stepsDownSoFar + stepsDown)
      val element = row.take(stepsRightSoFar + stepsRight + 1).toList.last
      if (element.equals("#"charAt(0))) {
        loop(data, stepsRight, stepsDown, stepsRightSoFar + stepsRight, stepsDownSoFar + stepsDown, treesSoFar + 1)
      } else {
        loop(data, stepsRight, stepsDown, stepsRightSoFar + stepsRight, stepsDownSoFar + stepsDown, treesSoFar)
      }
    }
  }

  lazy val part1Test = loop(day3Test, 3,1, 0 ,0 ,0)
  lazy val part1Data = loop(day3Data, 3,1, 0 ,0 ,0)
  println(s"Part 1 Test hit ${part1Test} trees")
  println(s"Part 1 Data hit ${part1Data} trees")

  lazy val part2_a = loop(day3Data, 1,1, 0 ,0 ,0)
  lazy val part2_b = part1Data
  lazy val part2_c = loop(day3Data, 5,1, 0 ,0 ,0)
  lazy val part2_d = loop(day3Data, 7,1, 0 ,0 ,0)
  lazy val part2_e = loop(day3Data, 1,2, 0 ,0 ,0)

  println(s"Part 2 a hit ${part2_a} trees")
  println(s"Part 2 b hit ${part2_b} trees")
  println(s"Part 2 c hit ${part2_c} trees")
  println(s"Part 2 d hit ${part2_d} trees")
  println(s"Part 2 e hit ${part2_e} trees")
  println(s"... all multiplied together is ${part2_a * part2_b * part2_c * part2_d * part2_e}")


}
