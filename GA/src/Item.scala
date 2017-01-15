/** Item of a knapsack.
 *  @constructor Constructs an item with given name, mass and value
 *  @param name         Name of the item
 *  @param mass         Weight of the item
 *  @param value        Value of the item
 */
class Item(val name: String, val mass: Double, val value: Double) {
  /** Ratio value / mass of the item. */
  val ratio = value / mass
  
  
  /** Returns a string representing the item. */
  override def toString = name 
}





/** Item of a knapsack. */
object Item {
  /** Generates a random item.
   *  Mass and value of item are randomly generated using a Gaussian
   *  distribution of given mean and variance equal to 25% of mass and
   *  value.
   *  @param meanMass   Mean mass
   *  @param meanValue  Mean value
   */
  def randomize(meanMass: Double, meanValue: Double): Item = {
    val r = util.Random
    val mass  = r.nextGaussian * (meanMass * 0.25)  + meanMass
    val value = r.nextGaussian * (meanValue * 0.25) + meanValue
    val name  = "Random item [" + value.formatted("%.2f") + "|" + mass.formatted("%.2f") + "]"
    new Item(name, mass, value)
  }
  
  
  /** Generates a random set of items.
   *  Every item has randomly generated mass and value (@see randomize).
   *  @param size Number of items to generate
   *  @param meanMass   Mean mass
   *  @param meanValue  Mean value
   *  @return Set of randomly generated items
   */
  def randomList(size: Int, mass: Double, value: Double): List[Item] = 
    List.fill(size)(randomize(mass, value))
  
  
  /** Pre-generated list of items. */
  final val Items = randomList(20, 0.5, 5.0)
}