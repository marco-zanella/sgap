package sgap

/** Genetic Algorithm solver.
 *  @construct Construct an instance of a solver
 *  @tparam A           Type of population
 *  @param population   Initial population
 *  @param size         Size of the population
 *  @param crossover    Probability of a crossover
 *  @param mutation     Probability of a mutation
 *  @param iteration    Number of iterations
 */
class GASolver[A](
    var population: Population[A],
    val size: Int,
    val crossover: Double,
    val mutation: Double,
    val iterations: Int) {
  /** Auxiliary constructor.
   *  Uses default values for crossover and mutation probabilities.
   *  @param population Initial population
   *  @param size       Maximum size of the population
   *  @param iteration  Number of iterations
   */
  def this(population: Population[A], size: Int, iterations: Int) =
    this(population, size, GASolver.Crossover, GASolver.Mutation, iterations)
  
  
  /** Auxiliary constructor.
   *  Uses size of initial population as size and default values for
   *  crossover and mutation probabilities.
   *  @param population Initial population
   *  @param iteration  Number of iterations
   */
  def this(population: Population[A], iterations: Int) =
    this(population, population.size, iterations)
  
  
  /** Computes Genetic Algorithm and returns best individual.
   *  @return Best individual
   */
  def solve =
    population.best :: List.fill(iterations){
      population = population nextGeneration (size, crossover, mutation)
      population.best
    } maxBy (_.fitness)
  
  
  /** Computes a solution and saves statistics into a csv file.
   * @param file        Path to output file
   * @return Best individual
   */
  def solveAndLog(file: String = "out.dat") = {
    val out = new java.io.FileWriter(file)
    out.write("#Iter\tSize\tAverage\tMedian\tBest\tWorst\tRange\tVariance\n")
    val best = population.best :: List.tabulate(iterations)(i => {
      population = population nextGeneration (size, crossover, mutation)
      val info = List(
          population.size,
          population.averageFitness,
          population.medianFitness,
          population.bestFitness,
          population.worstFitness,
          population.range,
          population.fitnessVariance)
      .map ("\t" + _) reduce (_ + _)
      out write (i + info + "\n")
      population.best
    })
    out.close
    best maxBy (_.fitness)
  }
}





/** Companion object for the Genetic Algorithm Solver class.
 *  Defines standard parameters.
 */
object GASolver {
  /** Default crossover probability. */
  final val Crossover  = 0.8
  
  /** Default mutation probability. */
  final val Mutation   = 0.3
}