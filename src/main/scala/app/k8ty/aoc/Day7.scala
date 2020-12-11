package app.k8ty.aoc

object Day7 extends App with DataLoader {

  lazy val data = loadData("day_7.data").toList
  // The general format is {color} bags contain # {color} bag(s), ...
  // e.g. faded lavender bags contain 1 light salmon bag, 2 dotted purple bags, 5 pale gold bags, 3 muted gold bags.
  // with the exception of
  // {color} bags contain no other bags.

  // Remove bags that can't contain other bags
  lazy val canContainBags: Seq[String] = data.filter(!_.endsWith("contain no other bags."))

  // Get the color of bags that can directly contain a particular color of bag
  def canContainColor(color: String) = {
    canContainBags.map(_.split("bags contain")).filter(_(1)contains(color)).map(_(0).trim)
  }

  def loop(colorList: Seq[String], finalColors: Set[String]): Int = colorList.length match {
    case 0 => finalColors.size
    case _ => {
      val nextColors = for {
        color <- colorList
        extraColors <- canContainColor(color)
      } yield extraColors
      loop(nextColors, finalColors ++ nextColors.toSet)
    }
  }

  val canHasShinyGold = canContainColor("shiny gold")

  println(s"There are ${loop(canHasShinyGold, canHasShinyGold.toSet)} different bags that could have shiny gold")

  // shiny gold bags contain 2 dark coral bags, 1 mirrored orange bag.
  // => Seq[("dark coral", 2),("mirrored orange", 1)]
  def containsColorCount(color: String): Seq[(String, Int)] = {
    val rule = data.filter(_.startsWith(color)).head
    val bags = rule.split("bags contain")(1).replaceAll("\\.","").split(",").map(_.trim)
    val bagCounts = bags.map {
      case none if none.equals("no other bags") => ("NONE", 0)
      case other => (other.split(" ").slice(1,3).mkString(" "), other.head.toString.toInt)
    }
    bagCounts.toSeq.filterNot(_._1.equals("NONE"))
  }

  def loop2(color: String): Int = {
    val subColors = containsColorCount(color)
    subColors.map(t => t._2 + t._2 * loop2(t._1)).sum
  }

  println(s"There are ${loop2("shiny gold")} bags in bags")
}
