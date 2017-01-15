import sgap._
import greedy._
import benchmark._

/** Workbench for the Knapsack 1-0 problem.
 *  This is just to show how the sgap package work.
 */
object KnapsackWorkbench {
  //////////////////////////////////////////////////////////////////////
  // Problem configuration.
  val capacity = 2.0
  val items    = Item.Items
  
  // Tool to quickly print a solution
  def prettyPrint(name: String, solution: Knapsack): Unit =
    println((String format ("%-26s", name)) + solution)
  
  println("----------------------------------------------------------------------")
  println("Analysis of the Knapsack 1-0:\n")
  
  
  
  //////////////////////////////////////////////////////////////////////
  // Relaxed solution
  println((String format ("%-26s", "Relaxation:")) + "[" +
      ((items maxBy { _.ratio }).ratio * capacity).formatted("%.2f") +
      " | " + (capacity formatted ("%.2f")) + "]")
  
  
  
  //////////////////////////////////////////////////////////////////////
  // Greedy approaches
  val candidates = items
  val feasible = (a: List[Item]) => (a map (_.mass)).sum <= capacity
  
  // Tool to quickly compute a solution
  def greedySolution(select: List[Item] => Item) = {
    val solution = greedy.solve(candidates, select, feasible)
    val toBool = List.tabulate(items.size)(x => solution contains items(x))
    new Knapsack(toBool, capacity, items)
  }
  
  
  // Greedy approach #1: most valuable first
  val byValue  = (a: List[Item]) => a maxBy (_.value)
  val g1       = greedySolution(byValue)
  prettyPrint("Greedy by Value:", g1)
  
  
  // Greedy approach #2: lightest first
  val byMass   = (a: List[Item]) => a minBy (_.mass)
  val g2       = greedySolution(byMass)
  prettyPrint("Greedy by Mass:", g2)
  
  
  // Greedy approach #3: best ratio first
  val byRatio  = (a: List[Item]) => a maxBy (_.ratio)
  val g3       = greedySolution(byRatio)
  prettyPrint("Greedy by Ratio:", g3)
  
  
  
  //////////////////////////////////////////////////////////////////////
  // Genetic Algorithm approaches
  val size     = 64
  val iter     = 200
  val initial  = Knapsack.randomSet(items, capacity, size): List[GAKnapsack]
  val initialG = (g3: GAKnapsack) :: initial.tail
  
  // Tool to quickly compute a solution
  def GASolution(individuals: List[GAKnapsack], elitism: Boolean = false) = {
    val population = if (elitism)
      new Population(individuals) with Elitism[List[Boolean]]
    else
      new Population(individuals)
    val solver = new GASolver(population, iter)
    new Knapsack(solver.solve.genes, capacity, items)
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