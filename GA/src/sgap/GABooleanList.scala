package sgap
import crossovers.OnePoint
import mutations.Flip

/** Individual whose genetic information is a boolean list.
 *  Gives a ready-to-use trait which only needs evaluator and constructor
 *  functions. Genetic information is a boolean list.
 */
trait GABooleanList
extends Individual[List[Boolean]]
with OnePoint[Boolean]
with Flip;