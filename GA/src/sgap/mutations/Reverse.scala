package sgap
package mutations

/** Individual who performs a mutation reversing information.
 * Requires a genetic information in the form of List[A].
 * @tparam A Type of the list (genetic information) 
 */
trait Reverse[A] extends Individual[List[A]]{
  /** Mutates genetic information.
   *  Reverses given genetic information.
   *  @return A new genetic information
   */
  override val mutation = (genes: List[A]) => genes.reverse
}