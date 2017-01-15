package sgap
package crossovers

/** Individual who performs a single information exchange.
 * Requires a genetic information in the form of List[A].
 * @tparam A            Type of the list (genetic information)
 */
trait Exchange[A] extends Individual[List[A]] {
  /** Mixes up genetic informations.
   *  Mixes genetic information of this individual with another one
   *  exchanging a single information.
   *  @param that       Other individual
   */
  override val crossover = (a: List[A], b: List[A]) => {
    val p = random.nextInt(a.size)
    if (random.nextBoolean)
      a updated (p, b(p))
    else
      b updated (p, a(p))
  }
}