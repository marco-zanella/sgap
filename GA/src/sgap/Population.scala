package sgap

/** A population of individuals.
 *  @construct Constructs a population
 *  @tparam A           Type of the individual
 *  @param population   List of individuals
 */
class Population[A](population: List[Individual[A]]) {
  /** Auxiliary constructor accepting a tuple instead of a list.
   *  @param individuals Tuple of individuals
   */
  def this(individuals: Individual[A]*) = this(individuals.toList)
  
  
  /** Individuals of the population. */
  val individuals = population
  
  /** Least-fitting individual in the population. */
  val worst = individuals minBy (_.fitness)
  
  /** Best-fitting individual in the population. */
  val best = individuals maxBy (_.fitness)
  
  /** Highest fitness in the population. */
  val bestFitness = best.fitness
  
  /** Lowest fitness in the population. */
  val worstFitness = worst.fitness
  
  /** Fitness range. */
  val range = bestFitness - worstFitness
  
  /** Global fitness of the population. */
  val fitness = bestFitness
  
  /** Number of individuals in the population. */
  val size = individuals.size
  
  /** Average fitness of individuals in the population. */
  val averageFitness = (individuals map (_.fitness) sum) / size
  
  /** Mean fitness of individual in the population. */
  val meanFitness = averageFitness
  
  /** Median fitness of individuals in the population. */
  lazy val medianFitness = (individuals map (_.fitness) sorted) apply (size / 2)
  
  /** Variance of fitness values. */
  lazy val fitnessVariance =
    (individuals map (x => math.pow(x.fitness - meanFitness, 2))).sum / size
  
  /** Random number generator. */
  val random = util.Random
  
  
  /** Returns normalized fitness of an individual.
   *  Normalized fitness ranges from 0.0 (least-fitting) to 1.0
   *  (best-fitting).
   *  @param individual Individual to be used
   *  @return Normalized fitness in [0, 1]
   */
  def relativeFitness(individual: Individual[A]) = {
    if(range != 0)
      (individual.fitness - worstFitness) / range
    else
      1.0
  }
  
  
  /** Randomly selects an individual.
   *  The individual is selected randomly.
   *  @return A randomly selected individual
   */
  def sample = individuals(random nextInt size)
  
  
  /** Randomly selects an individual.
   *  Individuals with higher fitness have higher chances to be
   *  selected. Selection algorithm is a variant of the roulette-wheel.
   *  @return A randomly selected individual
   */
  def select = {
    val p = random.nextDouble
    individuals filter (x => relativeFitness(x) >= p) minBy (_.fitness)
  }
  
  
  /** Randomly selects two individuals.
   *  @see select
   *  @return Two randomly selected individuals
   */
  def selectParents = (select, select)
  
  
  /** Mates two individuals from this population.
   *  @param crossover  Crossover probability
   *  @return A new individual
   */
  def mate(crossover: Double) = {
    val (p1, p2) = selectParents
    if (random.nextDouble <= crossover)
      p1 +- p2
    else
      if (random.nextBoolean) p1 else p2
  }
  
  
  /** Returns a new individual.
   *  New individual is generated mating two individuals from this
   *  population. Mutation may occur.
   *  @param crossover  Crossover probability
   *  @param mutation   Mutation probability
   *  @return A new individual
   */
  def offspring(crossover: Double, mutation: Double) = {
    val newborn = mate(crossover)
    if (random.nextDouble <= mutation) newborn ^ else newborn
  }
  
  
  /** Returns a new population.
   *  New population is generated mating individuals from this
   *  population.
   *  @param size       Size of new population
   *  @param crossover  Crossover probability
   *  @param mutation   Mutation probability
   *  @return A new population
   */
  def nextGeneration(size: Int, crossover: Double, mutation: Double) =
    new Population(List.fill(size)(offspring(crossover, mutation)))
  
  
  /** Returns a string representing the population. */
  override def toString = individuals.toString
}