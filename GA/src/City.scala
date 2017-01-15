/** A city in the Traveling Salesman Problem.
 *  @param name         Name of the city
 *  @param x            X-coordinate of the city
 *  @param y            Y-coordinate of the city
 */
class City(val name: String, val x: Double, val y: Double) {
  /** Returns squared distance between this and that city.
   *  @param that       The other city
   */
  def distanceSqr(that: City) =
    Math.pow(x - that.x, 2) + math.pow(y - that.y, 2)
  
  
  /** Returns distance between this and that city.
   *  @param that       The other city
   */
  def distance(that: City) = math.sqrt(distanceSqr(that))
  
  
  /** Returns a string representing the city.
   *  String is in the format "name @[x, y]".
   */
  override def toString = name + " @[" + x + ", " + y + "]"
}





/** City in the Traveling Salesman Problem. */
object City {
  /** Generates a random city.
   *  City is positioned between (0, maxX) and (0, maxY).
   *  @param maxX       Maximum x-coordinate
   *  @param maxY       Maximum y-coordinate
   */
  def randomize(maxX: Double, maxY: Double) = {
    val r = util.Random
    new City("Random City", r.nextDouble * maxX, r.nextDouble * maxY)
  }
  
  
  /** Generates a random set of cities.
   *  Each city lays between (0, maxX) and (0, maxY).
   *  @param size       Number of cities to generate
   *  @param maxX       Maximum x-coordinate
   *  @param maxY       Maximum y-coordinate
   */
  def randomSet(size: Int, maxX: Double, maxY: Double) =
    List.fill(size)(randomize(maxX, maxY))
}