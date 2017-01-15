package sgap
package mutations

/** Individual who performs a mutation flipping one bit of information.
 *  Requires a genetic information in the form of List[Boolean].
 */
trait Flip extends Individual[List[Boolean]] {
  /** Mutates genetic information.
   *  Flips information at a random position of the genetic information
   *  (List[Boolean]).
   *  @return A new genetic information
   */
  override val mutation = (genes: List[Boolean]) => {
    val p = random.nextInt(genes.size)
    genes updated (p, !genes(p))
  }
}