package sgap
package crossovers

/** Individual who performs a two point crossover.
 * Requires a genetic information in the form of List[A].
 * @tparam A            Type of the list (genetic information)
 */
trait TwoPoint[A] extends Individual[List[A]] {
  /** Mixes up genetic informations.
   *  Mixes genetic information of current individual with another one using a
   *  two points crossover.
   *  @param that       Other individual
   *  @return A new genetic information
   */
  override val crossover = (a: List[A], b: List[A]) => {
    val p1 = random.nextInt(a.size)
    val p2 = random.nextInt(a.size - p1)
    val (a1, aA) = a  splitAt p1
    val (a2, a3) = aA splitAt p2
    val (b1, bB) = b  splitAt p1
    val (b2, b3) = bB splitAt p2
    if(random.nextBoolean) a1 ++ b2 ++ a3 else b1 ++ a2 ++ b3
  }
}