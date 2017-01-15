import Item._

/** Knapsack 1-0.
 *  @constructor Constructs a knapsack with given content and capacity
 *  @param items        Items in the knapsack
 *  @param capacity     Maximum weight
 *  @param choices      Available items
 */
class Knapsack(val items: List[Boolean], val capacity: Double, val choices: List[Item]) {
  /** Content as list of items. */
  val content =
    (0 until items.size) filter (items(_)) map (choices(_))
  
  /** Weight of the content. */
  val weight = (content map (_.mass)).sum
  
  /** Value of the content. */
  val value = if (weight <= capacity) content map (_.value) sum else 0.0
  
  
  /** Returns a string representing the knapsack.
   *  Output is in the form "[value | weight]".
   */
  override def toString =
    "[" + value.formatted("%.2f") + " | " + weight.formatted("%.2f") + "]"
}





/** Companion object for Knapsack 1-0. */
object Knapsack {
  /** Generates knapsack with random content.
   *  Content is not guaranteed to be feasible.
   *  @param items      Available items
   *  @param capacity   Capacity of the knapsack
   */
  def randomize(items: List[Item], capacity: Double): Knapsack = {
    val r = util.Random
    new Knapsack(List.fill(items.size)(r.nextBoolean), capacity, items)
  }
  
  
  /** Generates a set of randomized knapsacks.
   *  @see randomize
   *  @param items      Available items
   *  @param size       Number of knapsacks
   */
  def randomSet(items: List[Item], capacity: Double, size: Int): List[Knapsack] = 
    List.fill(size)(randomize(items, capacity))
}