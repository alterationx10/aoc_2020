package app.k8ty.aoc

object Day9 extends App with DataLoader {

  val testData = loadData("day_9.test").map(_.toDouble).toSeq
  val liveData = loadData("day_9.data").map(_.toDouble).toSeq

  def xploit(preamble: Seq[Double], remaining: Seq[Double]): Double = remaining match {
    case Nil => throw new Exception("unXploitable")
    case h::t => {
      val combo = preamble.combinations(2).map(_.sum)
      if (combo.contains(h)) {
        xploit(preamble.drop(1) :+ h, t)
      } else {
        h
      }
    }
  }

  val weakTestNumber = xploit(testData.take(5), testData.drop(5))
  val weakLiveNumber = xploit(liveData.take(25), liveData.drop(25))

  println(s"Part 1 test has vulnerability at $weakTestNumber")
  println(s"Part 1 live has vulnerability at ${weakLiveNumber}")

  def sumsTo(target: Double, sum: Double, contiguous: Seq[Double], data: Seq[Double]): (Boolean, Seq[Double]) = (target == sum) match {
    case true => (true, contiguous)
    case false => data match {
      case Nil => (false, contiguous)
      case h::t => {
        val newSum = sum + h
        newSum match {
          case _ if newSum == target => (true, contiguous :+ h)
          case _ if newSum > target => (false, contiguous :+ h)
          case _ => sumsTo(target, newSum, contiguous :+ h, t)
        }
      }
    }
  }

  def findWhereSumsTo(target: Double, data: Seq[Double]): Seq[Double] = data match {
    case Nil => throw new Exception(":sad_trombone:")
    case _ => {
      val result = sumsTo(target, 0, Seq.empty, data)
      if (result._1) {
        result._2
      } else {
        findWhereSumsTo(target, data.tail)
      }
    }
  }

  val part2Test = findWhereSumsTo(weakTestNumber, testData)
  val part2Live = findWhereSumsTo(weakLiveNumber, liveData)


  println(s"Part 2 test is weak at ${part2Test.min + part2Test.max}")
  println(s"Part 2 live is weak at ${part2Live.min + part2Live.max}")


}
