import sgap._

/** Knapsack adapted for Genetic Algorithms.
 *  @constructor Creates a knapsack to be used in genetic algorithms
 *  @param genes        List of items (genetic information)
 *  @param capacity     Capacity of the knapsack
 *  @param choices      Available items
 */
class GAKnapsack(val genes: List[Boolean], capacity: Double, choices: List[Item])
extends Knapsack(genes, capacity, choices)
with GABooleanList {
  /** Evaluator function. */
  override val evaluator = (a: List[Boolean]) => value
  
  
  /** Constructs a GAKnapsack.
   *  @param genes      Genetic information
   */
  def construct(genes: List[Boolean]): GAKnapsack =
    new GAKnapsack(genes, capacity, choices)
}





/** Knapsack adapted for Genetic Algorithms. */
object GAKnapsack {
  /** Converts a Knapsack to a GAKnapsack.
   *  @param value      Knapsack
   */
  implicit def toGAKnapsack(value: Knapsack): GAKnapsack =
    new GAKnapsack(value.items, value.capacity, value.choices)
  
  
  /** Converts a list of Knapsack to a list of GAKnapsack.
   *  @param value      List of Knapsack
   */
  implicit def toGAKnapsack(value: List[Knapsack]) : List[GAKnapsack] =
    value map (toGAKnapsack(_))
}