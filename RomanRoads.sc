import java.util.Date
import java.text.SimpleDateFormat
import java.text.DateFormat
import java.text.ParseException

object RomanRoads {

  case class Record(numberPlate: String, dateTime: Date, enter: Boolean, kilometers: Int)

  println("Welcome to the Scala worksheet")
  processInputFile(radixSort)("input.txt")
  processInputFile(quickSort)("input.txt")

  def quickSort(numberPlate: List[String]): List[String] = numberPlate match {
    case Nil => { println("1"); Nil }
    case x :: Nil => x :: Nil
    case x :: xs => {
      val (smaller, bigger) = xs.partition(y => y < x)
      quickSort(smaller) ::: x :: quickSort(bigger)
    }
  }

  def radixSort(numberPlate: List[String]): List[String] = {
    var bucket = new Array[List[Array[Char]]](36)
    //  una lista para todo el alfabeto A-Za-z0-9 tamano:
    //  Array de listas
    //  convertir en un array de characters
    val conversionTable = collection.immutable.HashMap(
      '0' -> 0, '1' -> 1, '2' -> 2, '3' -> 3, '4' -> 4, '5' -> 5, '6' -> 6,
      '7' -> 7, '8' -> 8, '9' -> 9, 'A' -> 10, 'B' -> 11, 'C' -> 12, 'D' -> 13,
      'E' -> 14, 'F' -> 15, 'G' -> 16, 'H' -> 17, 'I' -> 18, 'J' -> 19, 'K' -> 20,
      'L' -> 21, 'M' -> 22, 'N' -> 23, 'O' -> 24, 'P' -> 25, 'Q' -> 26, 'R' -> 27,
      'S' -> 28, 'T' -> 29, 'U' -> 30, 'V' -> 31, 'W' -> 32, 'X' -> 33, 'Y' -> 34, 'Z' -> 35)

    var charList = numberPlate.map(s => s.toCharArray.reverse)
    val maxLength: Int = charList.foldLeft(0)((i, s) => i max s.length)

    // cada elemento de charList
    for (j <- 0 until maxLength) {
      charList.foreach(c => if (c.length > j) {
        c :: bucket(conversionTable(c(j)))
      })
      // borrar charList
      charList = Nil;
      // tomar cada elemento
      bucket.foreach(l => l.reverse.foreach(c => c :: charList))
      charList = charList.reverse
      // reverse
      // tomar cada elemto de esta lista
      // anadirlo a una charList

    }

    // repeat haste no hay mas caracters
    //   tomar el final del array
    //   ponerlos en la lista adecuada
    //   combinar los listas
  }

  def processInputFile(sortFunction: List[String] => List[String])(fileName: String) {
    def parseFares(fare: String): Array[Int] = fare.split(" ").map(s => s.toInt)
    def parseRecords(lines: scala.io.BufferedSource, n: Integer) = {
      if (n > 1) {
        val fares = parseFares(lines.takeWhile(c => c.isDigit || c == ' ').toString)
        def parseRecord(trips: List[String]) {
          val nextTrip = lines.takeWhile(c => c.isLetterOrDigit || c.isSpaceChar || c == ':').toString.trim()
          if (nextTrip.length == 0) { calcFares(sortFunction)(fares, trips) }
          else parseRecord(nextTrip :: trips)
        }
      }
    }

    val lines = scala.io.Source.fromFile(fileName)
    val noOfTestCases = lines.takeWhile(c => c.isDigit).foldLeft(0)((oldN, newN) => oldN * 10 + newN)
    if (noOfTestCases > 0) {
      lines.takeWhile(c => c.isDigit);
      parseRecords(lines, noOfTestCases);
    }
  }

  def calcFares(sortFunction: List[String] => List[String])(fareTable: Array[Int], trips: List[String]) = {
    def parseTrip(trip: String): Record = {
      //numberplate datetime enter/exit kilometros
      val tripSplit = trip.split(" ")
      val df = new SimpleDateFormat("MM:dd:HH:mm")
      //         numberplate   Date                    enter|exit               kilometros
      new Record(tripSplit(0), df.parse(tripSplit(1)), tripSplit(2) == "enter", tripSplit(3).toInt)

    }
    def getPairs(records: List[Record], pairs: List[Record]): List[Record] = records match {
    //TODO nilcase?
      case x :: y :: xs => if (x.numberPlate == y.numberPlate && x.enter && !y.enter) {
        getPairs(xs, x :: y :: pairs)
      } else { getPairs(y :: xs, pairs) }
    }
    val parsedSortedTripsPairs = getPairs(sortFunction(trips).map(parseTrip), Nil)
   //horas * lookup ingreso
   //TODO complete
    parsedSortedTripsPairs
  }

  val sortMe = "bla" :: "foo" :: "bar" :: Nil;
  val sortTwo = List("bla123", "foo145", "bar123", "145bla", "143bla", "10", "11")

  quickSort(sortMe)
  quickSort(sortTwo)

}