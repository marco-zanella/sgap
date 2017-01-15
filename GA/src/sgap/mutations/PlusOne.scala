package sgap
package mutations

/** Individual who performs a mutation increasing by one value of a locus.
 *  Requires a genetic information in the form of List[Int].
 */
trait PlusOne extends Individual[List[Int]] {
  /** Mutates genetic information.
   *  Increases by one the value stored in a randomly chosen position of
   *  the genetic information (List[Int]).
   *  @return A new genetic information
   */
  override val mutation = (genes: List[Int]) => {
    val p = random.nextInt(genes.size)
    genes.updated(p, genes(p) + 1)
  }
}