package app.k8ty.aoc

object Day4 extends App with DataLoader {


  val passportSeparator = "---"

  lazy val testData = loadData("day_4.test")
    .map(str => if (str.isEmpty) passportSeparator else str)
    .mkString(" ")
    .split(passportSeparator)

  lazy val liveData = loadData("day_4.data")
    .map(str => if (str.isEmpty) passportSeparator else str)
    .mkString(" ")
    .split(passportSeparator)

  val requiredFields = List(
    "byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"
  )

  def isValidPart1: String => Boolean = str => {
    requiredFields
      .map(f => str.contains(f))
      .forall(_ == true)
  }

  lazy val validTestDataPart1 = testData.filter(isValidPart1)
  lazy val validLiveDataPart1 = liveData.filter(isValidPart1)

  println(s"Part 1 test has ${validTestDataPart1.length} valid passports")
  println(s"Part 1 data has ${validLiveDataPart1.length} valid passports")

  def isValidPart2: String => Boolean = str =>
    str.trim.split(" ").map {
      case f if f.startsWith("byr:") => {
        val year = f.split(":")(1).toInt
        year >= 1920 && year <= 2002
      }
      case f if f.startsWith("iyr:") => {
        val year = f.split(":")(1).toInt
        year >= 2010 && year <= 2020
      }
      case f if f.startsWith("eyr:") => {
        val year = f.split(":")(1).toInt
        year >= 2020 && year <= 2030
      }
      case f if f.startsWith("hgt:") => {
        val height = f.split(":")(1)
        height.takeRight(2) match {
          case "in" => {
            val h = height.take(height.length - 2).toInt
            h >= 59 && h <= 76
          }
          case "cm" => {
            val h = height.take(height.length - 2).toInt
            h >= 150 && h <= 193
          }
          case _ => {
            false
          }
        }
      }
      case f if f.startsWith("hcl:") => {
        val hairColor = f.split(":")(1)
        val regex = "^[0-9a-f]+$".r
        hairColor.startsWith("#") &&
          hairColor.length == 7 &&
          regex.matches(hairColor.tail)
      }
      case f if f.startsWith("ecl:") => {
        val eyeColor = f.split(":")(1)
        val validEyeColors = Set(
          "amb", "blu", "brn",
          "gry", "grn", "hzl",
          "oth")
        validEyeColors.contains(eyeColor)
      }
      case f if f.startsWith("pid:") => {
        val passportId = f.split(":")(1)
        val regex = "^[0-9]+$".r
        passportId.length == 9 &&
          regex.matches(passportId)
      }
      case _ => true
    }.forall(_ == true)

  lazy val validLiveDataPart2 = validLiveDataPart1.filter(isValidPart2)

  println(s"Part 2 data has ${validLiveDataPart2.length} valid passports")

}
