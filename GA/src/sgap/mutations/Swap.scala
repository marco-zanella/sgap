package sgap
package mutations

/** Individual who performs a mutation swapping two genetic informations.
 *  Requires a genetic information in the form of List[A].
 *  @tparam A Type of the list (genetic information)
 */
trait Swap[A] extends Individual[List[A]]{
  /** Mutates genetic information.
   *  Swaps two elements in the genetic information.
   *  @return A new genetic information
   */
  override val mutation = (genes: List[A]) => {
    val (p1, p2) = (random.nextInt(genes.size), random.nextInt(genes.size))
    genes updated (p1, genes(p2)) updated (p2, genes(p1))
  }
}