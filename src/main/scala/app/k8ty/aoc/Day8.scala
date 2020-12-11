package app.k8ty.aoc

object Day8 extends App with DataLoader {

  lazy val testData: Seq[String] = loadData("day_8.test").toSeq
  lazy val liveData: Seq[String] = loadData("day_8.data").toSeq

  def loop(data: Seq[String], index: Int, visited: Set[Int], acc: Int): (Int, Set[Int]) = {
    if (visited.contains(index)) {
      (acc, visited)
    } else {
      val op = data(index)
      val split = op.split(" ")
      split(0) match {
        case "nop" => loop(data, index + 1, (visited + index), acc )
        case "acc" => loop(data, index + 1, (visited + index), acc + split(1).toInt )
        case "jmp" => loop(data, index + split(1).toInt, (visited + index), acc)
        case _ => throw new Exception(":sad_trombone:")
      }
    }
  }

  println(s"Part 1 test blows up after ${loop(testData, 0, Set.empty, 0)._1}")
  println(s"Part 1 live blows up after ${loop(liveData, 0, Set.empty, 0)._1}")


  def loop2(data: Seq[String], index: Int, swap: Int, visited: Set[Int], acc: Int): Option[Int] = {
    if (index == data.length) {
      return Option(acc)
    }
    if (visited.contains(index)) {
      None
    } else {
      val op = data(index)
      val split = op.split(" ")
      split(0) match {
        case "nop" => {
          if (index == swap) {
            // jmp instead
            loop2(data, index + split(1).toInt, swap, (visited + index), acc)
          } else {
            loop2(data, index + 1, swap, (visited + index), acc )
          }
        }
        case "acc" => loop2(data, index + 1, swap, (visited + index), acc + split(1).toInt )
        case "jmp" => {
          if (index == swap) {
            // nop instead
            loop2(data, index + 1, swap, (visited + index), acc )
          } else {
            loop2(data, index + split(1).toInt, swap, (visited + index), acc)
          }
        }
        case _ => throw new Exception(":sad_trombone:")
      }
    }
  }

  def seeWhatWorks(data: Seq[String]): Set[Int] = for {
    swap <- loop(data, 0, Set.empty, 0)._2
    maybe <- loop2(data, 0, swap, Set.empty, 0)
  } yield  maybe


  println(s"Part 2 test should have one solution in ${seeWhatWorks(testData)}")
  println(s"Part 2 live should have one solution in ${seeWhatWorks(liveData)}")
}
