package sgap
package crossovers

/** Individual who performs a single point crossover.
 * Requires a genetic information in the form of List[A].
 * @tparam A            Type of the list (genetic information)
 */
trait OnePoint[A] extends Individual[List[A]] {
  /** Mixes up genetic informations.
   *  Mixes genetic information of current individual with another one using a
   *  single point crossover.
   */
  override val crossover = (a: List[A], b: List[A]) => {
    val p = random.nextInt(genes.size)
    val (a1, a2) = a splitAt p
    val (b1, b2) = b splitAt p
    if(random.nextBoolean) a1 ++ b2 else b1 ++ a2
  }
}