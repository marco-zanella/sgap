package sgap

/** Individual of a population.
 *  An individual carries its genetic information as well as methods to
 *  manipulate it.
 *  @tparam A           Type of the genetic information
 */
trait Individual[A] {
  /** Genetic information. */
  val genes: A
  
  /** Crossover function. */
  val crossover: (A, A) => A
  
  /** Mutation function. */
  val mutation: A => A
  
  /** Function returning score of the genetic information. */
  val evaluator: A => Double
  
  /** Random number generator. */
  val random = util.Random
  
  
  /** Builds a new individual.
   *  Follows the Factory Method design pattern.
   *  @param genes      Genetic information of the new individual
   *  @return A new individual
   */
  def construct(genes: A): Individual[A]
  
  
  /** Mixes up genetic information of two individuals into a new one.
   *  @param that       Other individual
   *  @return A new individual
   */
  def +-(that: Individual[A]) = construct(crossover(genes, that.genes))
  
  
  /** Mutates an individual.
   *  @return A new individual with mutated genetic information
   */
  def ^ = construct(mutation(genes))
  
  
  /** Fitness of this individual. */
  lazy val fitness = evaluator(genes)
  
  
  /** Returns a string representing the individual. */
  override def toString = genes.toString 
}