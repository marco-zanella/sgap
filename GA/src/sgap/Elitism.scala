package sgap

/** A population where best individual is preserved. */
trait Elitism[A] extends Population[A] {
  /** Returns a new population.
   *  New population is generated mating individuals from this
   *  population. Best individual is preserved as it is.
   *  @param size       Size of new population
   *  @param crossover  Crossover probability
   *  @param mutation   Mutation probability
   *  @return A new population
   */
  override def nextGeneration(size: Int, crossover: Double, mutation: Double) = {
    val individuals = best :: List.fill(size - 1)(offspring(crossover, mutation))
    new Population(individuals) with Elitism[A]
  }
}