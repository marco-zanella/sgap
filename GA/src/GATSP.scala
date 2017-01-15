import sgap._

/** Traveling Salesman Problem adapted for Genetic Algorithms.
 *  @constructor Constructs a knapsack to be used in a genetic algorithm
 *  @param genes        Path among the cities (genetic information)
 */
class GATSP(val genes: List[City])
extends TSP(genes)
with GAList[City] {
  /** Evaluator function. */
  override val evaluator = (path: List[City]) =>
    if (TSP.feasible(path))
      1.0 / totalDistance
    else
      0.0
  
  
  /** Constructs a GATSP.
   *  @param genes      Genetic information
   */
  def construct(genes: List[City]): GATSP = new GATSP(genes)
}





/** Traveling Salesman Problem adapted for Genetic Algorithms. */
object GATSP {
  /** Converts a TSP to a GATSP.
   *  @param value      TSP
   */
  implicit def toGATSP(value: TSP): GATSP = new GATSP(value.path)
  
  
  /** Converts a list of TSP to a list of GATSP.
   *  @param value      list of TSP
   */
  implicit def toGATSP(value: List[TSP]): List[GATSP] =
    value map (toGATSP(_))
}