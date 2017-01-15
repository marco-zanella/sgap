/** Test application for the sgap package.
 * Simply performs tests one after another.
 */
object GA extends App {
  // Performs tests using the Knapsack 1-0 problem.
  KnapsackWorkbench
  
  // Performs tests using the Traveling Salesman Problem.
  TSPWorkbench
}