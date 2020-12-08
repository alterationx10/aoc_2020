package app.k8ty.aoc

object Day2 extends App with DataLoader {

  lazy val day2Data = loadData("day_2.data")
    .map(_.split(":"))
    .map { case Array(a, b) => (a, b.trim) }
    .toSeq

  def parseRule(rule: String): (Int, Int, Char) = {
    val ruleArr = rule.split(" ")
    val letter = ruleArr(1).charAt(0)
    val conditionArray = ruleArr(0).split("-")
    (conditionArray(0).toInt, conditionArray(1).toInt, letter)
  }

  def rule2CountF(rule: String): String => Boolean = {
    val conditions = parseRule(rule)
    (str: String) => {
      val count = str.count(_.equals(conditions._3))
      count >= conditions._1 && count <= conditions._2
    }
  }

  def rule2PositionF(rule: String): String => Boolean = {
    val conditions = parseRule(rule)
    (str: String) => {
      List(
        str.charAt(conditions._1 - 1).equals(conditions._3),
        str.charAt(conditions._2 - 1).equals(conditions._3)
      ).count(_ == true) == 1
    }
  }

  lazy val part1 = day2Data
    .map(ab => rule2CountF(ab._1).apply(ab._2))
    .count(_ == true)

  lazy val part2 = day2Data
    .map(ab => rule2PositionF(ab._1).apply(ab._2))
    .count(_ == true)

  println(s"Part 1 has ${part1} valid password")
  println(s"Part 2 has ${part2} valid password")

}
