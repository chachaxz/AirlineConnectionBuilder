import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object ConnectionBuilder extends App {
  // initialize adjacency list
  val itineraries = Itinerary.initialize

  val scanner = new java.util.Scanner(System.in)

  println("Please enter query no:")
  println("(1) Cost of a given itinerary")
  println("(2) List all possible itineraries with various constraint")
  println("(3) Find the cheapest itinerary")
  print("Select: ")
  val choice = scanner.nextLine()

  choice match {
    case "1" => println("Select [1] Cost of a given itinerary")
      print("Please enter itinerary e.g. A-B-C : ")
      val route = scanner.nextLine()
      println("route = " + route)
      Cost.calculate(route, itineraries) match {
        case Right(result) => println("Cost = " + result)
        case Left(error) => println("Error: "+ error)
      }

    case "2" => println("Select [2] List all possible itineraries with various constraint")
      println("Please enter itinerary: ")
      print("From = ")
      val from = scanner.nextLine()
      print("To = ")
      val to = scanner.nextLine()
      println("Please select constraints:")
      println("- Only itineraries with up to X legs. If no, X = 0 :")
      print("X = ")
      val legs = scanner.nextLine()
      println("- Only itineraries with up to Y Amadollars. If no, Y = 0 :")
      print("Y = ")
      val amount = scanner.nextLine()
      println("- Only itineraries that never travel the same leg twice:")
      print("(Y/N) = ")
      val isNoSameLeg = scanner.nextLine()

      def intToOption(n: Int): Option[Int] = if(n > 0) Option(n) else None
      def toIntOpt(s: String): Option[Int] = Try(s.toInt).toOption

      val result = (toIntOpt(legs), toIntOpt(amount), isNoSameLeg) match {
        case (Some(l), Some(a), "Y") => PossiblePath.find(itineraries, from, to, intToOption(l), intToOption(a), true)
        case (Some(l), Some(a), "N") => PossiblePath.find(itineraries, from, to, intToOption(l), intToOption(a), false)
        case _ => Left("Invalid input")
      }

      result match {
        case Right(value) => println("Number of possible itineraries = " + value.size)
          value.foreach(v => println(v._1 + ", cost = "+ v._2))
        case Left(error) => println("Error: "+ error)
      }

    case "3" => println("Select [3] Find the cheapest itinerary")
      println("Please enter itinerary: ")
      print("From = ")
      val from = scanner.nextLine()
      print("To = ")
      val to = scanner.nextLine()
      ShortestPath.find(itineraries, from, to) match {
        case Right(result) => println("Cheapest fare = " + result)
        case Left(error) => println("Error: "+ error)
      }

    case _ => println("'"+ choice + "' command not found.")
  }
}

final case class Itinerary(from: String, to: String, cost: Int)

object Itinerary {
  private def getList: List[Itinerary] = List(
    Itinerary("A", "B", 60),
    Itinerary("A", "C", 150),
    Itinerary("B", "C", 50),
    Itinerary("B", "E", 80),
    Itinerary("C", "B", 220),
    Itinerary("C", "G", 350),
    Itinerary("D", "I", 120),
    Itinerary("E", "A", 70),
    Itinerary("E", "C", 85),
    Itinerary("F", "A", 230),
    Itinerary("F", "G", 110),
    Itinerary("G", "F", 90),
    Itinerary("G", "H", 75),
    Itinerary("H", "I", 35),
    Itinerary("I", "C", 90),
    Itinerary("I", "D", 30)
  )

  def initialize: Map[String, List[Itinerary]] = {
    val destinations: List[Itinerary] = getList

    destinations.foldLeft(Map.empty[String, List[Itinerary]])((map, value) =>
      map + (value.from -> Try(map(value.from)).getOrElse(List()).+:(value).sortBy(_.to)))
  }

  def vertices: List[String] = initialize.keySet.toList.sorted
}

object Cost {
  def calculate(itinerary: String, list: Map[String, List[Itinerary]]): Either[String, Int] = {
    //Generate into (from,to) routes
    val itnr: List[String] = itinerary.split("-").map(_.toUpperCase().trim).toList

    if(itnr.size > 1) {
      val tuples: List[(String, String)] = itnr.sliding(2).map { case List(a, b) => (a, b) }.toList

      //Find all cost from given itinerary
      val routes: List[Option[Itinerary]] = tuples.map(t => list.get(t._1) match {
        case Some(value) => value.find(_.to == t._2)
        case None => None
      })

      if(routes.forall(_.isDefined))
        Right(routes.collect{ case Some(x) => x.cost }.sum) //calculate cost summary
      else
        Left("No such itinerary")
    } else
      Left("Invalid itinerary format")
  }
}

object PossiblePath {

  def find(routes: Map[String, List[Itinerary]], from: String, to: String,
           isXLegs: Option[Int] = None, isXAmount: Option[Int] = None,
           isNoSameLegTwice: Boolean = false): Either[String, mutable.Map[String, Int]] = {

    val pathCost: mutable.Map[String, Int] = mutable.Map[String, Int]()

    if(routes.isDefinedAt(from) || routes.isDefinedAt(to)) {
      //Add source to path
      val tempPath: Seq[String] = Seq(from)

      //Start recursive function
      recurFind(routes, from, to, tempPath, pathCost, isXLegs, isXAmount, isNoSameLegTwice)

      Right(pathCost)
    } else
      Left("No such itinerary")
  }

  def recurFind(list: Map[String, List[Itinerary]], s: String, d: String,
                tempPath: Seq[String], pathCost: mutable.Map[String, Int],
                isXLegs: Option[Int], isXAmount: Option[Int],
                isNoSameLegTwice: Boolean): Unit = {
    //Generate to itinerary format: A-B
    val itinerary: String = tempPath.mkString("-")

    //Same leg twice validation
    def isDuplicatedLeg: Boolean =
      if (tempPath.size > 1) {
        val duplicatedList = tempPath.sliding(2)
          .map { case List(a, b) => a + b }.toList
          .groupBy(identity).collect { case (x, y) if y.lengthCompare(1) > 0 => x }
        duplicatedList.nonEmpty
      } else false

    //Up to x amount validation
    def isUpToAmount(amount: Int) =
      if (tempPath.size > 1) {
        val cost: Int = calCost(itinerary)
        if (cost <= amount) true else false
      } else true

    //Up to x legs validation
    def isUpToLegs(legs: Int) = if(tempPath.sliding(2).size <= legs) true else false

    def calCost(path: String): Int = Cost.calculate(path, list).map(c => c).getOrElse(0)

    //Validate given conditions
    val isPassConds: Boolean = (isXLegs, isXAmount, isNoSameLegTwice) match {
      case (Some(legs), Some(amount), true) => isUpToLegs(legs) && isUpToAmount(amount) && !isDuplicatedLeg
      case (Some(legs), None, true) => isUpToLegs(legs) && !isDuplicatedLeg
      case (None, Some(amount), true) => isUpToAmount(amount) && !isDuplicatedLeg
      case (Some(legs), Some(amount), false) => isUpToLegs(legs) && isUpToAmount(amount)
      case (Some(legs), None, false) => isUpToLegs(legs)
      case (None, Some(amount), false) => isUpToAmount(amount)
      case (None, None, _) => !isDuplicatedLeg //Default case as no same leg twice
    }

    if (s == d && tempPath.size > 1) {
      if (isPassConds)
        pathCost += (itinerary -> calCost(itinerary)) //Save path
    }

    val adjacentList: List[Itinerary] = list.get(s).getOrElse(List.empty[Itinerary])

    for (adj <- adjacentList) {
      if (isPassConds) {
        //Add current node to path
        //Recursive find adjacent from current node
        recurFind(list, adj.to, d, tempPath :+ adj.to, pathCost, isXLegs, isXAmount, isNoSameLegTwice)
        tempPath.dropRight(1) //Remove current node from path
      }
    }
  }
}

object ShortestPath {

  def find(routes: Map[String, List[Itinerary]], from: String, to: String): Either[String, Int] = {
    val relaxation: mutable.Map[String, Int] = mutable.Map(Itinerary.vertices.map(v => (v, 0)): _*)

    if(routes.isDefinedAt(from) || routes.isDefinedAt(to))
      recurFind(routes, from, to, from, relaxation, Seq())
    else
      Left("No such itinerary")
  }

  @tailrec
  def recurFind(routes: Map[String, List[Itinerary]], from: String, to: String, v: String,
                relaxation: mutable.Map[String, Int], visited: Seq[String]): Either[String, Int] = {
    val visitedList: Seq[String] = visited :+ v

    if (v == to && relaxation(v) > 0)
      Right(relaxation(v))
    else {
      //Update edge relaxation
      routes(v).map { w =>
        val newCost = relaxation(v) + w.cost
        val preCost = relaxation(w.to)
        if (newCost < preCost || preCost == 0)
          relaxation(w.to) = newCost
      }

      //Ignore cycle
      val visitedV = if(from == to) visitedList.filterNot(_.contains(to)) else visitedList
      val compareList = relaxation.filterNot(r => visitedV.contains(r._1) || r._2 == 0)

      if(compareList.isEmpty)
        Left("No such itinerary")  //Visited all but not reach destination
      else{
        val min = compareList.minBy(_._2)
        recurFind(routes, from, to, min._1, relaxation, visitedList)
      }
    }
  }
}