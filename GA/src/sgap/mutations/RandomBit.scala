package sgap
package mutations

/** Individual who performs a mutation generating random information.
 *  Requires a genetic information in the form of List[Boolean].
 */
trait RandomBit extends Individual[List[Boolean]]{
  /** Mutates genetic information.
   *  Generates random genetic information (List[Boolean]).
   *  @return A new genetic information
   */
  override val mutation = (genes: List[Boolean]) =>
    List.fill(genes.size)(random.nextBoolean)
}