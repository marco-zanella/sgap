/** Traveling Salesman Problem.
 *  An instance of the TSP.
 *  @constructor Constructs a possible solution with given path among cities
 *  @param path         Path of the salesman
 */
class TSP(val path: List[City]) {
  /** List of pairs of cities in the path.
   *  For example, if path is [A, B, C, D], the list of pairs is
   *  [(A, B), (B, C), (C, D)].
   */
  lazy val pairs = {
    val t = path.tail
    (0 until t.size) map (x => (t(x), path(x)))
  }
  
  /** Total distance among cities in the path. */
  lazy val totalDistance = (pairs map (x => x._1 distance x._2)).sum
  
  /** Sum of squared distance among cities in the path.
   *  This should be preferred over totalDistance, when comparing total
   *  distances, for performance reasons.
   */
  lazy val totalDistanceSqr = (pairs map (x => x._1 distanceSqr x._2)).sum
  
  
  /** Returns a string representing the Salesman's path. */
  override def toString = "Distance travelled: " + totalDistance
}





/** Traveling Salesman Problem. */
object TSP {
  /** Randomly generates solution for the TSP.
   *  Solution is guaranteed to be feasible, but not to be optimal.
   *  @param cities     Available cities
   */
  def randomize(cities: List[City]) : TSP =
    new TSP(util.Random.shuffle(cities))
  
  
  /** Generates a set of random solutions.
   *  @see randomize
   *  @param size       Number of random solutions to generate
   *  @param cities     Available cities
   */
  def randomSet(size: Int, cities: List[City]) : List[TSP] =
    List.fill(size)(randomize(cities))
  
  
  /** Tells whether a path among cities is feasible.
   *  A path is feasible if and only if no city is visited more than
   *  once.
   *  @param path       Path among cities
   *  @return True if and only if every city is visited at most once
   */
  def feasible(path: List[City]): Boolean =
    path.size == path.distinct.size
}