package app.k8ty.aoc

object Day11 extends App with DataLoader {

  val testData: List[List[String]] = loadData("day_11.test").map(_.toList.map(_.toString)).toList
  val liveData: List[List[String]] = loadData("day_11.data").map(_.toList.map(_.toString)).toList

  def grabBlockFor(data: List[List[String]], i: Int, j: Int): List[List[String]] = {
    data.slice(i - 1, i + 2).map(_.slice(j - 1, j + 2))
  }

  def countOccupied(data: List[List[String]]): Int = {
    data.map(_.count(_.equals("#"))).sum
  }

  def seatOp(data: List[List[String]], seat: String): String = seat match {
    case "L" => {
      if (countOccupied(data) == 0) "#" else "L"
    }
    case "#" => {
      if (countOccupied(data) >= 5) "L" else "#"
    }
    case _ => seat
  }

  def iterate(data: List[List[String]]): List[List[String]] = {
    data.zipWithIndex.map { case (d, i) =>
      d.zipWithIndex.map { case (s, j) =>
        seatOp(grabBlockFor(data, i, j), s)
      }
    }
  }

  def part1(data0: List[List[String]], data1: List[List[String]]): Int = {
    if (data0 == data1) {
      return countOccupied(data0)
    }
    val iter = iterate(data0)
    part1(iter, data0)
  }

  println(s"Part 1 test: there are ${part1(testData, List.empty)} occupied seats")
  println(s"Part 1 live: there are ${part1(liveData, List.empty)} occupied seats")

  def flattenBlockFor(data: List[List[String]], i: Int, j: Int): List[List[String]] = {
    val aboveRows = data.take(i-1)
    val directlyAbove = aboveRows.map(_(j))

  }

}
