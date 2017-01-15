import sgap._
import greedy._
import benchmark._

/** Workbench for the Traveling Salesman Problem.
 *  This is just to show how the sgap package work.
 */
object TSPWorkbench {
  //////////////////////////////////////////////////////////////////////
  // Traveling Salesman Problem: Configuration
  val cities = City.randomSet(100, 10, 10)
  
  // Tool to quickly print a solution
  def prettyPrint(name: String, solution: TSP): Unit =
    println((String format ("%-26s", name)) + (solution.totalDistance formatted "%.2f"))
  
  println("----------------------------------------------------------------------")
  println("Analysis of the Traveling Salesman Problem:\n")
  
  
  
  //////////////////////////////////////////////////////////////////////
  // Random solution
  val randomSolution = TSP.randomize(cities)
  prettyPrint("Random solution:", randomSolution)
  
  
  
  //////////////////////////////////////////////////////////////////////
  // Greedy approaches
  val feasible = (a: List[City]) => true
  
  // Tool to quickly compute a solution
  def greedySolution(select: Selection[City]) =
    new TSP(greedy.solve(cities, select, feasible))
  
  
  // Greedy approach #1: randomly
  val randomly = (a: List[City]) => a(util.Random.nextInt(a.size))
  val greedy1  = greedySolution(randomly)
  prettyPrint("Greedy randomly:", greedy1)
  
  
  // Greedy approach #2: closest first
  val closestFirst = (solution: List[City], candidates: List[City]) => solution match {
    case a :: l => candidates minBy (_ distanceSqr a)
    case _      => randomly(candidates)
  }
  val greedy2 = greedySolution(closestFirst)
  prettyPrint("Greedy closest:", greedy2)
  
  
  // Greedy approach #3: farthest first
  val farthestFirst = (solution: List[City], candidates: List[City]) => solution match {
    case a :: l => candidates maxBy (_ distanceSqr a)
    case _      => randomly(candidates)
  }
  val greedy3 = greedySolution(farthestFirst)
  prettyPrint("Greedy farthest:", greedy3)
  
  
  // Greedy approach #4: one of the 5 closest first
  val closest5First = (solution: List[City], candidates: List[City]) => solution match {
    case a :: l => {
      val sorted = candidates sortBy (_ distanceSqr a)
      val idx    = util.Random.nextInt(5 min sorted.size)
      sorted(idx)
    }
    case _      => randomly(candidates)
  }
  val greedy4 = greedySolution(closest5First)
  prettyPrint("Greedy among closest 5:", greedy4)
  
  
  
  //////////////////////////////////////////////////////////////////////
  // Genetic Algorithm approaches
  val size     = 32
  val iter     = 500
  val initial  = TSP.randomSet(size, cities): List[GATSP]
  val initialG = (greedy2: GATSP) :: initial.tail
  
  // Tool to quickly compute a solution
  def GASolution(individuals: List[GATSP], elitism: Boolean = false) = {
    val population = if (elitism)
      new Population(individuals) with Elitism[List[City]]
    else
      new Population(individuals)
    val solver = new GASolver(population, iter)
    new TSP(solver.solve.genes)
  }
  
  
  // Genetic Algorithm #1: regular
  val ga1 = GASolution(initial)
  prettyPrint("Genetic Algorithm:", ga1)
  
  
  // Genetic Algorithm #2: elitism enabled
  val ga2 = GASolution(initial, true)
  prettyPrint("GA with Elitism:", ga2)
  
  
  // Genetic Algorithm #3: using greedy
  val ga3 = GASolution(initialG)
  prettyPrint("GA + Greedy:", ga3)
  
  
  // Genetic Algorithm #4: using greedy with elitism
  val ga4 = GASolution(initialG, true)
  prettyPrint("GA + Greedy with Elitism:", ga4)
}