/** Greedy algorithm implementation.
 *  Greedy strategies are (relative) simple algorithms which may be
 *  deployed to solve certain types of problems.
 *  Greedy solution are, in general, not optimal, but take very little
 *  time to be computed with a dynamic programming approach.
 *  A greedy algorithm requires a set of candidates to choose from,
 *  the solution built so far, a selection function, a feasibility
 *  checking predicate and a goal checking predicate.
 */
package object greedy {
  type Solution[A]   = List[A]
  type Candidates[A] = List[A]
  type NaiveSelection[A] = Candidates[A] => A
  type AwareSelection[A] = (Solution[A], Candidates[A]) => A
  type Selection[A] = Either[NaiveSelection[A], AwareSelection[A]]
  type Predicate[A] = Solution[A] => Boolean
  
  /** Default goal predicate.
   *  Always evaluates to false.
   */
  private def DefaultGoal[A] = (solution: Solution[A]) => false
  
  
  /** Default feasibility predicate.
   *  Always evaluates to true.
   */
  private def DefaultFeasible[A] = (solution: Solution[A]) => true
  
  
  /** Converts a selection function to the Solution type.
   *  @param value      Selection function which ignores solution built so far
   */
  implicit def toSelection[A](value: NaiveSelection[A]): Selection[A] =
    Left(value)
  
  
  /** Converts a selection function to the Solution type.
   *  @param value      Selection function aware of the solution built so far
   */
  implicit def toSelection[A](value: AwareSelection[A]): Selection[A] =
    Right(value)
  
  
  
  /** Solves a problem greedily.
   *  @tparam A         Polymorphic type
   *  @param candidates Set of candidates
   *  @param solution   Solution built so far
   *  @param select     Selection function
   *  @param feasible   Feasibility checking predicate
   *  @param goal       Goal checking predicate
   *  @return A feasible solution
   */
  def solve[A](
      candidates: Candidates[A],
      solution:   Solution[A],
      select:     Selection[A],
      feasible:   Predicate[A],
      goal:       Predicate[A]) : List[A] = {
    if (candidates.isEmpty || goal(solution))
      solution
    else {
      val candidate = select match {
        case Left(s)  => s(candidates)
        case Right(s) => s(solution, candidates)
      }
      val newCandidates = candidates filter (_ != candidate)
      val newSolution   = candidate :: solution
      if (feasible(newSolution))
        solve(newCandidates, newSolution, select, feasible, goal)
      else
        solve(newCandidates, solution, select, feasible, goal)
    }
  }
  
  
  
  /** Solves a problem greedily.
   *  Initial solution is the empty list, goal and feasibility
   *  predicates can be omitted.
   *  If feasibility predicate is omitted, solution is considered always
   *  feasible.
   *  If goal predicate is omitted, greedy will go on until candidate
   *  set becomes empty.
   *  @tparam A         Polymorphic type
   *  @param candidates Set of candidates
   *  @param select     Selection function
   *  @param feasible   Feasibility checking predicate
   *  @param goal       Goal checking predicate
   *  @return A feasible solution
   */
  def solve[A](
      candidates: Candidates[A],
      select:     Selection[A],
      feasible:   Predicate[A] = DefaultFeasible[A],
      goal:       Predicate[A] = DefaultGoal[A]) : List[A] =
    solve(candidates, Nil, select, feasible, goal)
}