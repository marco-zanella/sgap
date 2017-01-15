package sgap
package mutations

/** Individual who performs a mutation flipping every bit of information.
 *  Requires a genetic information in the form of List[Boolean].
 */
trait FlipAll extends Individual[List[Boolean]] {
  /** Mutates genetic information.
   *  Flips whole genetic information (List[Boolean]).
   *  @return A new genetic information
   */
  override val mutation = (genes: List[Boolean]) => genes map (!_)
}