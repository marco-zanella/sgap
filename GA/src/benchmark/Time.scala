package benchmark

/** Performs time related benchmarks.
 *  This is a poor-quality yet extremely simple way to measure time
 *  required by an instruction.
 *  Although it is not intended for serious benchmarking, it is precise
 *  enough for a preliminary study.
 */
object Time{
  /** Prints time required by an instruction.
   *  Time is expressed in milliseconds.
   *  @tparam A         Polymorphic type
   *  @param f          Piece of code to measure
   */
  def measure[A](f: => A) = {
    val start  = System.nanoTime
    val result = f
    val stop   = System.nanoTime
    println("Time: " + (stop - start) / 1e6 + "ms")
    result
  }
}