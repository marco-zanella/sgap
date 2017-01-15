package sgap
import crossovers.OnePoint
import mutations.Swap

/** Individual whose genetic information is a list.
 *  Gives a ready-to-use trait which only needs evaluator and constructor
 *  functions.
 *  @tparam A Type of the list holding genetic information
 */
trait GAList[A]
extends Individual[List[A]]
with OnePoint[A]
with Swap[A];