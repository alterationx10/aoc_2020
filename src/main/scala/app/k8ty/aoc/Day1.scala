package app.k8ty.aoc

object Day1 extends App with DataLoader {

  lazy val day1Test: Seq[Int] = loadData("day_1.test").map(_.toInt).toSeq
  lazy val day1Data: Seq[Int] = loadData("day_1.data").map(_.toInt).toSeq

  def solve(target: Int, nPairs: Int, data: Seq[Int]) = {
    data
      .combinations(nPairs)
      .find(_.sum == target)
      .map(_.product)
      .getOrElse(throw new Exception(":sad_trombone:"))
  }

  println(s"Part 1 Test: ${solve(2020, 2, day1Test)}")
  println(s"Part 1 Data: ${solve(2020, 2, day1Data)}")
  println(s"Part 2 Test: ${solve(2020, 3, day1Test)}")
  println(s"Part 2 Data: ${solve(2020, 3, day1Data)}")

}
